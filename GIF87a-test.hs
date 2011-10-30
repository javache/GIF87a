{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Data.Attoparsec (parseTest)
import qualified Data.ByteString as B
import System.Environment

import GIF87a.Parser

-- | 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  forM_ args (\path -> B.readFile path >>= parseTest parser)
