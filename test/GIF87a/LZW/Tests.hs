module GIF87a.LZW.Tests where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Test.QuickCheck
import Text.Printf

import GIF87a.LZW

data Payload = Payload Int [Word8] deriving (Show)

instance Arbitrary Payload where
  arbitrary = do
    initialCodeSize <- choose (2, 8)
    wordsLength <- choose (1, 1024)
    words <- replicateM wordsLength $
      fromIntegral <$> choose (0 :: Int, 2 ^ initialCodeSize - 1)
    return $ Payload initialCodeSize words

prop_encodeDecode :: Payload -> Bool
prop_encodeDecode (Payload codeSize words) =
  let lbs = encodeLZW codeSize words
      bs = B.concat $ BL.toChunks lbs
  in words == decodeLZW codeSize bs

tests = [ ("encode.decode/id", quickCheck prop_encodeDecode) ]
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
