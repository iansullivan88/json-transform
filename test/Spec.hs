{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.JSONTransform
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import GHC.Generics
import GHC.Stack
import Test.HUnit hiding(cases)
import System.Directory
import System.FilePath

data TransformTestSuite = TransformTestSuite { suiteTransform :: Value
                                             , cases :: Maybe [TransformTestCase] } deriving(Generic, Show)

instance FromJSON TransformTestSuite

data TransformTestCase = TransformTestCase { input :: Value
                                           , expected :: Maybe Value } deriving(Generic, Show)

-- Write a custom version of parseJSON becase expected: Null means something
-- different than expected: Nothing. This difference is respected by using the .:! operator
instance FromJSON TransformTestCase where
    parseJSON (Object t) = TransformTestCase <$>
                               t .:  "input" <*>
                               t .:! "expected"
    parseJSON invalid    = typeMismatch "TransformTestCase" invalid


main :: IO ()
main = buildTests >>= void . runTestTT

buildTests :: IO Test
buildTests = do
    c <- getCurrentDirectory
    let dir = c </> "test-cases"
    TestList <$> (listDirectoryFull dir >>= traverse buildTestCategory)

buildTestCategory :: FilePath -> IO Test
buildTestCategory dir = do
    ps <- listDirectoryFull dir
    ts <- traverse buildTest (filter ((".json" ==) . takeExtension) ps)
    pure $ TestLabel (takeBaseName dir) (TestList ts)

buildTest :: FilePath -> IO Test
buildTest fp = pure $ TestLabel fp $ TestCase $ do
    bs <- B.readFile fp
    suite <- maybe (assertFailure' "Could not parse json file") pure (decode' bs)
    let expectValid = isJust $ cases suite
    case parseTransform $ suiteTransform suite of
         Left m  -> assertBool ("The transform could not be parsed: " ++ T.unpack m) (not expectValid)
         Right t -> do cs <- maybe (assertFailure' "The transform was valid but there are no test cases") pure (cases suite)
                       forM_ cs (\c -> runTestCase t (expected c) (input c))

runTestCase :: JSONTransform -> Maybe Value -> Value -> IO ()
runTestCase t me i = handleResult (transform t i) me where
    handleResult (Left m) (Just _)  = assertFailure $ "Transform failed with message: " ++ T.unpack m
    handleResult (Right a) Nothing  = assertFailure $ "Expected transform to fail but got: " ++ show a
    handleResult (Right a) (Just e) = assertEqual "Transformed value should match expected" e a
    handleResult _ _                = pure ()

-- allows arbitrary return type which simplifies some code above
assertFailure' :: HasCallStack => String -> IO a
assertFailure' m = assertFailure m >> error "This should never be seen"


listDirectoryFull :: FilePath -> IO [FilePath]
listDirectoryFull dir = listDirectory dir >>= pure . fmap (dir </>) 
