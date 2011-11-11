module GIF87a.Transformations where

import Data.Either (either)
import Data.Word (Word8)

import GIF87a.Image

grayscale :: Image -> Image
grayscale img = img { globalColorMap = colors' }
  where colors' = fmap (map convertToGrayscale) $ globalColorMap img
        convertToGrayscale color =
          let luminance = round
                        $ (realToFrac $ red color :: Float) * 0.3
                        + (realToFrac $ green color :: Float) * 0.59
                        + (realToFrac $ blue color :: Float) * 0.11
          in Color { red = luminance, green = luminance, blue = luminance }

data Direction = Horizontal | Vertical
mirror :: Direction -> Image -> Image
mirror direction img =
  let descriptors' = map (either (Left . modify) Right) $ descriptors img
  in img { descriptors = descriptors' }
  where
    modify :: ImageDescriptor -> ImageDescriptor
    modify descr = descr { pixels = mirror' direction $ pixels descr }

    mirror' :: Direction -> [[Word8]] -> [[Word8]]
    mirror' Horizontal rows = reverse rows
    mirror' Vertical rows = map reverse rows
