{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.JSONTransform
    ( JSONTransform, transform, parseTransform
    ) where

import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Lazy as Map
import Data.List
import Data.Monoid
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Error

-- | A type to represent a transform from one json value to another
newtype JSONTransform = JSONTransform TValue

-- | Represents a key in a transform object. It can be a literal string or
-- an accessor to an array value
data KeyExpression = TextKey T.Text | GeneratorKey T.Text T.Text Accessor deriving(Show)

-- | Transform analog of Aeson Value. Instead of String values, there are expressions
data TValue = TObject ![(KeyExpression, TValue)]
            | TArray !(V.Vector TValue)
            | TExp ![ValueExp]
            | TNumber !Scientific
            | TBool !Bool
            | TNull deriving(Show)

-- | Represents a variable and list of keys
data Accessor = Accessor T.Text [T.Text] deriving(Show)

-- | A value expression which can either be a literal text value or
-- an Accessor which is a list of keys.
data ValueExp = TextValue T.Text | AccessorValue Accessor deriving(Show)

-- | A dictionary of values representing available variables in the current scope
type Dictionary = Map.HashMap T.Text Value

-- | Parses a json 'Value' and returns either a parse error or and error
-- about why parsing failed.
parseTransform :: Value -> Either T.Text JSONTransform
parseTransform v = JSONTransform <$> fromJSONValue v

-- | Transforms a json 'Value' using the supplied 'JSONTransform'
transform :: JSONTransform -> Value -> Either T.Text Value
transform (JSONTransform t) v = fromTValue (Map.singleton T.empty v) t

fromJSONValue :: Value -> Either T.Text TValue
fromJSONValue v = case v of
    (Object o) -> TObject <$> traverse parseKvp (Map.toList o)
    (Array vs) -> TArray <$> traverse fromJSONValue vs
    (String t) -> TExp <$> myParse valueExpParser t
    (Number n) -> Right $ TNumber n
    (Bool b)   -> Right $ TBool b
    Null       -> Right TNull
    where
        parseKvp (k, va) = (,) <$> myParse keyExpParser k <*> fromJSONValue va
        myParse p ex = case parse p "" ex of
                             (Left err)   -> Left $ formatError ex err
                             (Right ex') -> Right ex'

fromTValue :: Dictionary -> TValue -> Either T.Text Value
fromTValue d (TObject kvps) = (Object . Map.fromList) <$> traverse (fromKeyValuePair d) kvps
fromTValue d (TArray vs)    = Array <$> traverse (fromTValue d) vs
fromTValue d (TExp es)      = valueFromExpressions d es
fromTValue _ (TNumber n)    = Right $ Number n
fromTValue _ (TBool b)      = Right $ Bool b
fromTValue _ TNull          = Right Null

fromKeyValuePair :: Dictionary -> (KeyExpression, TValue) -> Either T.Text (T.Text, Value)
fromKeyValuePair d (TextKey k, t) = (k,) <$> fromTValue d t
fromKeyValuePair d (GeneratorKey k var a, t) = (k,) <$> arrayValue where
    arrayValue = do v' <- valueFromAccessor d a
                    case v' of
                         Array vs -> Array <$> traverse (\kValue -> fromTValue (Map.insert var kValue d) t) vs
                         _        -> Left $ T.concat ["Key generator '", showAccessor a, "' does not refer to array"]


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

valueFromExpressions :: Dictionary -> [ValueExp] -> Either T.Text Value
valueFromExpressions d es = case traverse (valueFromExpression d) es of
    (Left err) -> Left err
    (Right vs) -> case foldM combineJSONValue Null vs of
        Nothing -> Left "Could not combine JSON values"
        Just c  -> Right c

valueFromExpression :: Dictionary -> ValueExp -> Either T.Text Value
valueFromExpression _ (TextValue t)     = Right $ String t
valueFromExpression d (AccessorValue a) = valueFromAccessor d a

valueFromAccessor :: Dictionary -> Accessor -> Either T.Text Value
valueFromAccessor d a@(Accessor var ks) = do
    s <- maybe (Left $ T.append "Variable not in scope: " var) Right (Map.lookup var d)
    maybe (Left $ T.append "Can't read value: " (showAccessor a)) Right (valueForKeys s ks)


-- | Try to extract a 'Value' using a list of keys 
-- and a source json 'Value'
valueForKeys :: Value -> [T.Text] -> Maybe Value
valueForKeys o aks = go o ([],aks) where
    go v          (_,[])    = Just v
    go (Object m) (ps,k:ks) = maybe Nothing (\v -> go v (k:ps,ks)) (Map.lookup k m)
    go _ _                  = Nothing

showAccessor :: Accessor -> T.Text
showAccessor (Accessor var ks)
    | T.null var = formatKeys ks
    | otherwise  = T.concat ["$",var,"(",formatKeys ks,")"]

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
valueExpParser = many ((AccessorValue <$> try accessorParser) <|> (TextValue <$> literalParser)) <* eof

keyExpParser :: Parsec T.Text () KeyExpression
keyExpParser = do 
    key <- literalParser
    (eof *> pure (TextKey key)) <|> indexExp key where
        indexExp k = between (char '[') (char ']') (do
            var <- char '$' *> variableParser
            spaces >> string "<-" >> spaces
            acc <- accessorParser
            pure $ GeneratorKey k var acc) <* eof


accessorParser :: Parsec T.Text () Accessor
accessorParser = do void $ char '$'
                    var <- option T.empty variableParser
                    between (char '(') (char ')') $ do
                       ks <- sepBy1 literalParser (char '.')
                       return $ Accessor var ks

literalParser :: Parsec T.Text () T.Text 
literalParser = T.pack <$> many1 litChar where
    litChar = try (noneOf specialCharacters <|> (char '\\' *> oneOf specialCharacters))

variableParser :: Parsec T.Text () T.Text
variableParser = T.pack <$> many1 alphaNum

specialCharacters :: String
specialCharacters = "$()[]\\."
