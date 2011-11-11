module GIF87a.Transformations where

import Data.Either (either)
import Data.Word (Word8)

import GIF87a.Image

mapColors :: (ColorMapBlock -> ColorMapBlock) -> Image -> Image
mapColors f img = img { globalColorMap = colors' }
  where colors' = fmap (map f) $ globalColorMap img

mapImages :: (ImageDescriptor -> ImageDescriptor) -> Image -> Image
mapImages f img =
  let descriptors' = map (either (Left . f) Right) $ descriptors img
  in img { descriptors = descriptors' }

grayscale :: Image -> Image
grayscale img = mapColors convertToGrayscale img
  where convertToGrayscale color =
          let luminance = round
                        $ (realToFrac $ red color :: Float) * 0.3
                        + (realToFrac $ green color :: Float) * 0.59
                        + (realToFrac $ blue color :: Float) * 0.11
          in Color { red = luminance, green = luminance, blue = luminance }

data Direction = Horizontal | Vertical
mirror :: Direction -> Image -> Image
mirror direction img = mapImages modify img
  where
    modify :: ImageDescriptor -> ImageDescriptor
    modify descr = descr { pixels = mirror' direction $ pixels descr }

    mirror' :: Direction -> [[Word8]] -> [[Word8]]
    mirror' Horizontal rows = reverse rows
    mirror' Vertical rows = map reverse rows
