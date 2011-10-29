{-# LANGUAGE OverloadedStrings #-}
module GIF87a.Parser where

import qualified Data.Attoparsec as P
import System.Environment

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s
