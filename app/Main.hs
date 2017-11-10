module Main where

import           Data.JSONTransform

import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Monoid
import qualified Data.Text                  as T
import           Options.Applicative
import           System.IO

data Options = Options {
    inputPath     :: String,
    transformPath :: String
}

main :: IO ()
main = customExecParser (prefs $ mempty <> showHelpOnEmpty) (info optionParser mempty) >>= run

optionParser :: Parser Options
optionParser = Options <$> inParser <*> transParser where
    inParser = strOption $ long "input"
                     <> short 'i'
                     <> metavar "FILENAME"
                     <> help "a path to the input file or - to read from stdin"
    transParser = strOption $ long "transform"
                     <> short 't'
                     <> metavar "FILENAME"
                     <> help "a path to the transform file or - to read from stdin"

run :: Options -> IO ()
run opt = do
    i  <- readJson (inputPath opt)
    t  <- readJson (transformPath opt)
    jt <- runEitherT (parseTransform t)
    r  <- runEitherT (transform jt i)
    C.putStrLn $ encode r

readJson :: FilePath -> IO Value
readJson f = do j <- readFileOrStdIn f
                let v = eitherDecodeStrict' j
                runEither v

readFileOrStdIn :: String -> IO B.ByteString
readFileOrStdIn "-" = B.getContents
readFileOrStdIn f   = B.readFile f

writeError :: String -> IO ()
writeError = hPutStrLn stderr

runEitherT :: Either T.Text a -> IO a
runEitherT (Left e)  = runEither (Left $ T.unpack e)
runEitherT (Right x) = runEither (Right x)

runEither :: Either String a -> IO a
runEither (Left e)  = fail e
runEither (Right x) = pure x
