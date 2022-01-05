module Main where

import System.Environment
import Network.URI
import Lib
import Debug.Trace

main :: IO ()
main = do
  getArgs >>= traceShowM
  [target] <- getArgs
  let Just gUri = parseURI target >>= validateGeminiURI
  print =<< request gUri
