{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import System.Environment

import GIF87a.Image
import GIF87a.Parser

-- | 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  forM_ args (\path -> do
    input <- B.readFile path
    let result = parseOnly parser input
    case result of
      Left err  -> putStrLn err
      Right img -> let x = pixels $ head $ images img
                   in do putStrLn $ show img
                         putStrLn $ show (length x, map length x)
    )
