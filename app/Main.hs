module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment
import Text.Megaparsec (parseMaybe)
import Text.URI
import Lib
import Debug.Trace

main :: IO ()
main = do
  getArgs >>= traceShowM
  [target] <- getArgs
  let Just gUri = parseMaybe @Text parser (Text.pack target) >>= validateGeminiURI
  print =<< request gUri
