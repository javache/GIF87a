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


-- | Test: 1x1 black image
image :: Image
image = Header { signature = "GIF87a"
               , screenDescriptor = Screen { screenWidth = 1
                                           , screenHeight = 1
                                           , colorResolution = 8
                                           , bitsPerPixelS = 1
                                           , backgroundColorIndex = 0
                                           }
               , globalColorMap = Just
                   [ Color { red = 255, green = 255, blue = 255 }
                   , Color {red = 0, green = 0, blue = 0 }]
               , descriptors =
                   [ Left ( Image { imageLeft = 0
                                  , imageTop = 0
                                  , imageWidth = 1
                                  , imageHeight = 1
                                  , localColorMap = Nothing
                                  , interlaced = False
                                  , bitsPerPixelI = 1
                                  , pixels = [[ 1 ]]
                                  }
                    ) ]
               }
