{-# LANGUAGE OverloadedStrings #-}
module GIF87a.Parser (parser) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Word (Word8, Word16)
import Prelude hiding (take)

import Debug.Trace

import GIF87a.Image
import GIF87a.LZW

parser :: Parser Image
parser = do
  signature <- string "GIF87a"
  (screen, usesGlobalColorMap) <- parseScreenDescriptor
  colorMap <- parseColorMap usesGlobalColorMap (bitsPerPixelS screen)
  -- TODO: check for GIF Extension block
  images <- many1 parseImageDescriptor
  char ';' -- 0x3B

  return Header { signature = signature
                , screenDescriptor = screen
                , globalColorMap = colorMap
                , images = images
                }

parseScreenDescriptor :: Parser (ScreenDescriptor, Bool)
parseScreenDescriptor = do
  width <- parseWord16
  height <- parseWord16
  (colorMap, resolution, bitsPerPixel) <-
    take 1 >>= readBits (do
      m <- getBit
      r <- getAsWord8 3
      skip 1
      b <- getAsWord8 3
      return (m, r + 1, b + 1)
    )
  background <- parseWord8
  char '\NUL'
  return (Screen { screenWidth = width, screenHeight = height
                 , colorResolution = resolution
                 , bitsPerPixelS = bitsPerPixel
                 , backgroundColorIndex = background
                 }, colorMap)

parseColorMap :: Bool -> Word8 -> Parser (Maybe [ColorMapBlock])
parseColorMap p bitsPerPixel =
  if p then
      let n = 2 ^ bitsPerPixel
      in pure Just <*> count n parseColorMapBlock -- TODO: use vector
  else return Nothing

parseColorMapBlock :: Parser ColorMapBlock
parseColorMapBlock = do
  red <- parseWord8; green <- parseWord8; blue <- parseWord8
  return Color { red = red, green = green, blue = blue}

parseImageDescriptor :: Parser ImageDescriptor
parseImageDescriptor = do
  char ',' -- 0x2C
  left <- parseWord16
  top <- parseWord16
  width <- parseWord16
  height <- parseWord16
  (usesColorMap, interlaced, bitsPerPixel) <- take 1 >>=
    readBits (do
      m <- getBit
      i <- getBit
      skip 3
      b <- getAsWord8 3
      return (m, i, b + 1)
    )
  colorMap <- parseColorMap usesColorMap bitsPerPixel
  pixels <- parseRaster height width bitsPerPixel interlaced
  return Image { imageLeft = left, imageTop = top
               , imageWidth = width, imageHeight = height
               , interlaced = interlaced
               , bitsPerPixelI = bitsPerPixel
               , localColorMap = colorMap
               , pixels = pixels
               }

parseRaster :: Word16 -> Word16 -> Word8 -> Bool -> Parser [[Word8]]
parseRaster rows cols bits interlaced = do
  codeSize <- pure fromIntegral <*> parseWord8
  pure (chunk (fromIntegral cols) . concat) <*> manyTill (do
      blockSize <- pure fromIntegral <*> parseWord8
      pure (decodeLZW codeSize) <*> take blockSize
    ) (char '\NUL')
  where
    chunk :: Int -> [a] -> [[a]]
    chunk n [] = []
    chunk n xs = L.take n xs : chunk n (L.drop n xs)

parseWord8 :: Parser Word8
parseWord8 = B.head <$> take 1

parseWord16 :: Parser Word16
parseWord16 = take 2 >>= readBits getWord16le

readBits :: BitGet a -> ByteString -> Parser a
readBits reader input =
  case runBitGet input reader of
    Left msg -> fail msg
    Right n  -> return n
