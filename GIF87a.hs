{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (forM_, when)
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either (lefts)
import Data.List (nub)
import System.Console.CmdArgs
import System.Environment
import System.IO (stdout)

import qualified GIF87a.Encoder as Encoder
import GIF87a.Image
import qualified GIF87a.Parser as Parser
import GIF87a.Transformations

data GIF87a = GIF87a { encode :: Bool
                     , transform :: Bool
                     , parse :: Bool
                     , files :: [FilePath]
                     } deriving (Show, Data, Typeable)

options = GIF87a { encode    = def &= help "Re-encode the images"
                 , transform = def &= help "Apply transformations to each image"
                 , parse     = def &= help "Show the parsed image"
                 , files     = def &= args &= typ "FILES"
                 } &=
                 summary "GIF87a v0.1, (c) Pieter De Baets"

-- | Runs the main program
main :: IO ()
main = do
  args <- cmdArgs options
  forM_ (files args) (\path -> do
    input <- B.readFile path
    case parseOnly Parser.parser input of
      Left err  -> putStrLn err
      Right img -> do
        let img' = if transform args
                   then grayscale $ mirror Horizontal img
                   else img
        when (encode args) (BL.hPut stdout $ Encoder.encode img')
        when (parse args) $ do
          print img'
          let x = pixels $ head $ lefts $ descriptors img'
          print (length x, nub $ map length x)
    )
