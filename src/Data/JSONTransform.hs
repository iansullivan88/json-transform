{-# LANGUAGE OverloadedStrings #-}

module Data.JSONTransform
    ( JSONTransform, transform, parseTransform
    ) where

import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Monoid
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Error

-- | A type to represent a transform from one json value to another
newtype JSONTransform = JSONTransform TValue

-- | Transform analog of Aeson Value. Instead of String values, there are expressions
data TValue = TObject !(H.HashMap T.Text TValue)
            | TArray !(V.Vector TValue)
            | TExp ![ValueExp]
            | TNumber !Scientific
            | TBool !Bool
            | TNull deriving(Show)

-- | A value expression which can either be a literal text value or
-- an Accessor which is a list of keys.
data ValueExp = Literal T.Text | Accessor [T.Text] deriving(Show)

-- | Parses a json 'Value' and returns either a parse error or and error
-- about why parsing failed.
parseTransform :: Value -> Either T.Text JSONTransform
parseTransform v = JSONTransform <$> fromJSONValue v

-- | Transforms a json 'Value' using the supplied 'JSONTransform'
transform :: JSONTransform -> Value -> Either T.Text Value
transform (JSONTransform t) v = fromTValue v t

fromJSONValue :: Value -> Either T.Text TValue
fromJSONValue (Object o) = TObject <$> traverse fromJSONValue o
fromJSONValue (Array vs) = TArray <$> traverse fromJSONValue vs
fromJSONValue (String t) = either (Left . formatError t) (Right . TExp) (parse valueExpParser "" t)
fromJSONValue (Number n) = Right $ TNumber n
fromJSONValue (Bool b)   = Right $ TBool b
fromJSONValue Null       = Right TNull

fromTValue :: Value -> TValue -> Either T.Text Value
fromTValue v (TObject o) = Object <$> traverse (fromTValue v) o
fromTValue v (TArray vs) = Array <$> traverse (fromTValue v) vs
fromTValue v (TExp es)   = valueFromExpressions v es
fromTValue _ (TNumber n) = Right $ Number n
fromTValue _ (TBool b)   = Right $ Bool b
fromTValue _ TNull       = Right Null

formatError :: T.Text -> ParseError -> T.Text
formatError expression e = T.concat ["Error parsing accessor: "
                             , expression
                             , "\n"
                             , "at position "
                             , T.pack $ show $ sourceColumn $ errorPos e
                             , "\n"
                             , T.pack parsecError] where
    -- Taken from parsec source - format error without line number
    parsecError = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

valueFromExpressions :: Value -> [ValueExp] -> Either T.Text Value
valueFromExpressions v es = case traverse (valueFromExpression v) es of
    (Left err) -> Left err
    (Right vs) -> case foldM combineJSONValue Null vs of
        Nothing -> Left "Could not combine JSON values"
        Just c  -> Right c

valueFromExpression :: Value -> ValueExp -> Either T.Text Value
valueFromExpression _ (Literal t)   = Right $ String t
valueFromExpression v (Accessor ks) = maybe (Left $ T.append "Can't read value: " (formatKeys ks)) Right (valueForKeys v ks)

-- | Try to extract a 'Value' using a list of keys 
-- and a source json 'Value'
valueForKeys :: Value -> [T.Text] -> Maybe Value
valueForKeys o aks = go o ([],aks) where
    go v          (_,[])    = Just v
    go (Object m) (ps,k:ks) = maybe Nothing (\v -> go v (k:ps,ks)) (H.lookup k m)
    go _ _                  = Nothing

formatKeys :: [T.Text] -> T.Text
formatKeys = T.concat . intersperse "."

-- | Combines two json 'Value's into a single Value.
-- This might fail eg a bool and a number can't be 
-- sensibly combined
combineJSONValue :: Value -> Value -> Maybe Value
combineJSONValue v Null = Just v
combineJSONValue Null v = Just v
combineJSONValue (String t1) (String t2) = Just $ String $ t1 <> t2
combineJSONValue _ _ = Nothing

--------------
-- Parsing
--------------

valueExpParser :: Parsec T.Text () [ValueExp]
valueExpParser = many (try accessorParser <|> literalParser) <* eof

accessorParser :: Parsec T.Text () ValueExp
accessorParser = do void $ char '$'
                    between (char '(') (char ')') $ do
                       ks <- sepBy1 (many1 $ noneOf ").") (char '.')
                       return $ Accessor $ T.pack <$> ks

literalParser :: Parsec T.Text () ValueExp
literalParser = Literal . T.pack <$> litChars where
    litChars = many1 (try (string "$$" *> pure '$') <|> noneOf "$")
