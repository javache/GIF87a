{-# LANGUAGE OverloadedStrings #-}

module GIF87a.Encoder (encode) where

import Control.Monad (when, forM_)
import Data.Binary.BitPut
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word8)
import Prelude hiding (putChar)

import GIF87a.Image
import GIF87a.LZW

encode :: Image -> BL.ByteString
encode img = runBitPut $ do
  putByteString $ signature img
  encodeScreenDescriptor (screenDescriptor img)
                         (isJust $ globalColorMap img)
  when (isJust $ globalColorMap img) $
    forM_ (fromJust $ globalColorMap img) encodeColorMapBlock

encodeScreenDescriptor :: ScreenDescriptor -> Bool -> BitPut
encodeScreenDescriptor descr globalColorMap = do
  putBits    $ screenWidth descr
  putBits    $ screenHeight descr
  putBit       globalColorMap
  putNBits 3 $ colorResolution descr
  putBit       False
  putNBits 3 $ bitsPerPixelS descr
  putBits    $ backgroundColorIndex descr
  putChar '\NUL'

encodeColorMapBlock :: ColorMapBlock -> BitPut
encodeColorMapBlock color = forM_ [red, green, blue] (\f -> putBits $ f color)

encodeExtensionBlock :: ExtensionBlock -> BitPut
encodeExtensionBlock block = do
  putChar '!'
  putBits $
  putChar '\NUL'

encodeImageDescriptor :: ImageDescriptor -> BitPut
encodeImageDescriptor img = do
  putChar ','

encodeRaster :: [[Word8]] -> Bool -> BitPut
encodeRaster rows interlaced = do
  putChar '\NUL'

putChar :: Char -> BitPut
putChar c = putBits (fromIntegral $ digitToInt c :: Word8)
