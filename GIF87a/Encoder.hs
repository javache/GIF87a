{-# LANGUAGE OverloadedStrings #-}

module GIF87a.Encoder (encode) where

import Control.Monad (when, forM_)
import Data.Binary.Put
import Data.Binary.BitPut hiding (putByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word8, Word16)
import Prelude hiding (putChar)

import GIF87a.Image
import GIF87a.LZW

encode :: Image -> BL.ByteString
encode img = runPut $ do
  putByteString $ signature img
  encodeScreenDescriptor (screenDescriptor img)
                         (isJust $ globalColorMap img)
  forM_ (fromMaybe [] $ globalColorMap img) encodeColorMapBlock
  forM_ (descriptors img) (\block -> case block of
      Left a  -> encodeImageDescriptor (screenDescriptor img) a
      Right b -> encodeExtensionBlock b
    )
  putChar ';'

encodeScreenDescriptor :: ScreenDescriptor -> Bool -> Put
encodeScreenDescriptor descr globalColorMap = do
  putWord16le $ screenWidth descr
  putWord16le $ screenHeight descr
  putLazyByteString $ runBitPut $ do
    putBit globalColorMap
    putNBits 3 $ colorResolution descr - 1
    putBit False
    putNBits 3 $ bitsPerPixelS descr - 1
  putWord8 $ backgroundColorIndex descr
  putChar '\NUL'

encodeColorMapBlock :: ColorMapBlock -> Put
encodeColorMapBlock color = forM_ [red, green, blue] (\f -> putWord8 $ f color)

encodeExtensionBlock :: ExtensionBlock -> Put
encodeExtensionBlock block = do
  putChar '!'
  putWord8 $ functionCode block
  forM_ (dataBytes block) (\block -> do
      putWord8 $ fromIntegral $ B.length block
      putByteString block
    )
  putChar '\NUL'

encodeImageDescriptor :: ScreenDescriptor -> ImageDescriptor -> Put
encodeImageDescriptor screen img = do
  putChar ','
  putWord16le $ imageLeft img
  putWord16le $ imageTop img
  putWord16le $ imageWidth img
  putWord16le $ imageHeight img
  putLazyByteString $ runBitPut $ do
    putBit $ isJust $ localColorMap img
    putBit $ False
    putNBits 3 (0 :: Word8)
    putNBits 3 $ bitsPerPixelI img - 1
  forM_ (fromMaybe [] $ localColorMap img) encodeColorMapBlock
  encodeRaster screen img

encodeRaster :: ScreenDescriptor -> ImageDescriptor -> Put
encodeRaster screen img = do
  -- if there's no local color table, use the global bits/pixels
  let codeSize = max 2 $ if (isJust $ localColorMap img)
                         then bitsPerPixelI img
                         else bitsPerPixelS screen
      encoded = encodeLZW (fromIntegral codeSize) (concat $ pixels img)
  putWord8 codeSize
  forM_ (chunk 254 encoded) (\block -> do
      putWord8 $ fromIntegral $ BL.length block
      putLazyByteString block
    )
  putChar '\NUL'
  where
    chunk :: Int64 -> BL.ByteString -> [BL.ByteString]
    chunk n xs
      | BL.null xs = []
      | otherwise  = let (head, tail) = BL.splitAt n xs
                     in head : chunk n tail

putChar :: Char -> Put
putChar = putWord8 . fromIntegral . ord
