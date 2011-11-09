module GIF87a.LZW (encodeLZW, decodeLZW) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word64)

import Debug.Trace

-- | Encode a list of bytes in a bytestring using LZW
encodeLZW :: [Word8] -> ByteString
encodeLZW = undefined

-- | Decode a bytestring using LZW to a list of bytes
decodeLZW :: Int -> ByteString -> [Word8]
decodeLZW initialCodeSize input =
  case runBitGet (reverseBytes input) initDecode of
    Left msg -> error msg
    Right x  -> x
  where
    clearCode = fromIntegral $ 2 ^ initialCodeSize :: Word16
    endOfInfoCode = clearCode + 1 :: Word16

    initDecode :: BitGet [Word8]
    initDecode = decode (initialAlphabet endOfInfoCode) []

    decode :: Alphabet -> [Word8] -> BitGet [Word8]
    decode alphabet previous = do
      bytes <- getLeftByteString $ codeSize alphabet
      evaluate $ case B.unpack $ reverseBytes bytes of
                   [x]   -> fromIntegral x
                   [x,y] -> (fromIntegral y `shiftL` 8) .|. fromIntegral x
      where
        evaluate :: Word16 -> BitGet [Word8]
        evaluate code
              | code == clearCode     = initDecode
              | code == endOfInfoCode = return []
              | otherwise             =
                  let new       = lookupAlphabet alphabet previous code
                      alphabet' = extendAlphabet alphabet previous new
                  in (new ++) `liftM` decode alphabet' new

-- * Alphabet methods

-- TODO: use a suitable datatype for alphabet
data Alphabet = Lookup [[Word8]] Int deriving (Show)

initialAlphabet :: Word16 -> Alphabet
initialAlphabet a = Lookup [[fromIntegral x] | x <- [0 .. a]]
                           (fromIntegral $ a + 1)

extendAlphabet :: Alphabet -> [Word8] -> [Word8] -> Alphabet
extendAlphabet alphabet@(Lookup codes max) prev new =
  if null prev || max >= 4096 then alphabet
  else Lookup (codes ++ [prev ++ [head new]]) (max + 1)

lookupAlphabet :: Alphabet -> [Word8] -> Word16 -> [Word8]
lookupAlphabet (Lookup codes max) previous index =
  let index' = fromIntegral index :: Int
  in if index' < max then codes !! index'
     else previous ++ [head previous]

codeSize :: Alphabet -> Int
codeSize (Lookup _ max) = min 12 $ floor (logBase 2 $ fromIntegral max) + 1

-- * Utilities

-- | Reverse the order bits of each byte in the bytestring
-- source: http://graphics.stanford.edu/~seander/bithacks.html
reverseBytes :: ByteString -> ByteString
reverseBytes = B.map (\b -> fromIntegral
               $ (`shiftR` 32)
               $ (* 0x0101010101)
               $ (.&. 0x0884422110)
               $ (* 0x80200802) (fromIntegral b :: Word64))
