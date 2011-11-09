module GIF87a.LZW (encodeLZW, decodeLZW) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Word (Word8, Word16, Word64)

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

data Alphabet = Lookup (Map Word16 [Word8]) Word16 deriving (Show)

initialAlphabet :: Word16 -> Alphabet
initialAlphabet a =
  let values = [(x, [fromIntegral x]) | x <- [0 .. a - 1]]
  in Lookup (M.fromList values) (a + 1)

extendAlphabet :: Alphabet -> [Word8] -> [Word8] -> Alphabet
extendAlphabet alphabet@(Lookup codes max) prev new =
  if null prev || max >= 4096 then alphabet
  else Lookup (M.insert max (prev ++ [head new]) codes) (max + 1)

lookupAlphabet :: Alphabet -> [Word8] -> Word16 -> [Word8]
lookupAlphabet (Lookup codes max) previous index =
  fromMaybe (previous ++ [head previous])
            (M.lookup (fromIntegral index) codes)

codeSize :: Alphabet -> Int
codeSize (Lookup _ max) = min 12 $ floor (logBase 2 $ fromIntegral max) + 1

-- * Utilities

-- | Reverse the order of bits of each byte in the bytestring
-- source: http://graphics.stanford.edu/~seander/bithacks.html
reverseBytes :: ByteString -> ByteString
reverseBytes = B.map (\b -> fromIntegral
               $ (`shiftR` 32)
               $ (* 0x0101010101)
               $ (.&. 0x0884422110)
               $ (* 0x80200802) (fromIntegral b :: Word64))
