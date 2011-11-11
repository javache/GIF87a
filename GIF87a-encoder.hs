{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Environment
import System.IO (stdout)

import GIF87a.Encoder
import GIF87a.Image
import GIF87a.Parser

-- | Runs the main program
main :: IO ()
main = do
  args <- getArgs
  forM_ args (\path -> do
    input <- B.readFile path
    case parseOnly parser input of
      Left err  -> putStrLn err
      Right img -> BL.hPut stdout $ encode img
    )
