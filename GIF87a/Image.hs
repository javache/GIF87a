module GIF87a.Image where

import Data.ByteString (ByteString)
import Data.Word (Word8, Word16)

data Image = Header { signature :: ByteString
                    , screenDescriptor :: ScreenDescriptor
                    , globalColorMap :: Maybe [ColorMapBlock]
                    , descriptors :: [Either ImageDescriptor ExtensionBlock]
                    } deriving (Eq, Show)

data ScreenDescriptor = Screen { screenWidth :: Word16
                               , screenHeight :: Word16
                               , colorResolution :: Word8
                               , bitsPerPixelS :: Word8
                               , backgroundColorIndex :: Word8
                               } deriving (Eq, Show)

data ImageDescriptor = Image { imageLeft :: Word16
                             , imageTop :: Word16
                             , imageWidth :: Word16
                             , imageHeight :: Word16
                             , localColorMap :: Maybe [ColorMapBlock]
                             , interlaced :: Bool
                             , bitsPerPixelI :: Word8
                             , pixels :: [[Word8]]
                             } deriving (Eq, Show)

data ExtensionBlock = Extension { functionCode :: Word8
                                , dataBytes :: [ByteString]
                                } deriving (Eq, Show)

data ColorMapBlock = Color { red :: Word8
                           , green :: Word8
                           , blue :: Word8
                           } deriving (Eq, Show)
