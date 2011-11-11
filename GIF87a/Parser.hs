{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module GIF87a.Parser (parser) where

import Control.Applicative ((<$>), (<|>))
import Data.Attoparsec.Char8
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Word (Word8, Word16)
import Prelude hiding (take)

import GIF87a.Image
import GIF87a.LZW

parser :: Parser Image
parser = do
  signature <- string "GIF87a"
  (screen, usesGlobalColorMap) <- parseScreenDescriptor
  colorMap <- parseColorMap usesGlobalColorMap (bitsPerPixelS screen)
  blocks <- many1 $ (Left <$> parseImageDescriptor)
                  <|> (Right <$> parseExtensionBlock)
  char ';'  -- 0x3B

  return Header { signature = signature
                , screenDescriptor = screen
                , globalColorMap = colorMap
                , descriptors = blocks
                }

parseScreenDescriptor :: Parser (ScreenDescriptor, Bool)
parseScreenDescriptor = do
  width <- parseWord16
  height <- parseWord16
  (colorMap, resolution, bitsPerPixel) <- take 1 >>=
    readBits (do
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
  if p then let n = 2 ^ bitsPerPixel
            in Just <$> count n parseColorMapBlock
       else return Nothing

parseColorMapBlock :: Parser ColorMapBlock
parseColorMapBlock = do
  red <- parseWord8; green <- parseWord8; blue <- parseWord8
  return Color { red = red, green = green, blue = blue}

parseExtensionBlock :: Parser ExtensionBlock
parseExtensionBlock = do
  char '!'   -- 0x21
  function <- parseWord8
  bytes <- manyTill (parseWord8 >>= take . fromIntegral) (char '\NUL')
  return Extension { functionCode = function, dataBytes = bytes }

parseImageDescriptor :: Parser ImageDescriptor
parseImageDescriptor = do
  char ','   -- 0x2C
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
  codeSize <- fromIntegral <$> parseWord8
  bytes <- decodeLZW codeSize . B.concat
    <$> manyTill (parseWord8 >>= take . fromIntegral) (char '\NUL')
  let rows = chunk (fromIntegral cols) bytes
  return $ if interlaced then interlace rows (length rows) else rows
  where
    chunk :: Int -> [a] -> [[a]]
    chunk n [] = []
    chunk n xs = L.take n xs : chunk n (L.drop n xs)

    interlace :: forall a. [a] -> Int -> [a]
    interlace rows height = map snd $ M.toAscList $ pass1 M.empty rows 0
      where
        pass1 = pass 8 (pass2, 4)
        pass2 = pass 8 (pass3, 2)
        pass3 = pass 4 (pass4, 1)
        pass4 = pass 2 (const . const, 0)

        pass :: Int -> (M.Map Int a -> [a] -> Int -> M.Map Int a, Int)
             -> M.Map Int a -> [a] -> Int -> M.Map Int a
        pass incr (next, offset) rows ls@(l : lr) i
          | i >= height = next rows ls offset
          | otherwise   = pass incr (next, offset)
                               (M.insert i l rows) lr (i + incr)
        pass incr (next, offset) rows [] i = rows

-- * Utilities

parseWord8 :: Parser Word8
parseWord8 = take 1 >>= readBits getWord8

parseWord16 :: Parser Word16
parseWord16 = take 2 >>= readBits getWord16le

readBits :: BitGet a -> ByteString -> Parser a
readBits reader input =
  case runBitGet input reader of
    Left msg -> fail msg
    Right n  -> return n
