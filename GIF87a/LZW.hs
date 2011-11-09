module GIF87a.LZW (encodeLZW, decodeLZW) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word64)

import Debug.Trace

encodeLZW :: [Word8] -> ByteString
encodeLZW = undefined

 -- TODO: use a suitable datatype for alphabet
data Alphabet = Lookup [[Word8]] Int deriving (Show)

decodeLZW :: Int -> [ByteString] -> [Word8]
decodeLZW initialCodeSize input =
  case runBitGet (reverseBytes $ B.concat input) initDecode of
    Left msg -> error msg
    Right x  -> x
  where
    clearCode = fromIntegral $ 2 ^ initialCodeSize :: Word16
    endOfInfoCode = clearCode + 1 :: Word16

    initDecode :: BitGet [Word8]
    initDecode = decode initialAlphabet []

    decode :: Alphabet -> [Word8] -> BitGet [Word8]
    decode alphabet previous = do
      let bitsToRead = codeSize alphabet
      remainingBits <- remaining
      if remainingBits < bitsToRead
        then return []
        else do
          bytes <- getLeftByteString bitsToRead
          evaluate $ case B.unpack $ reverseBytes bytes :: [Word8] of
                       [x]   -> fromIntegral x
                       [x,y] -> (fromIntegral y `shiftL` 8) .|. fromIntegral x
     where evaluate code
             | code == clearCode     = initDecode
             | code == endOfInfoCode = return []
             | otherwise             =
                 let value = lookup alphabet previous code
                     alphabet' = extendAlphabet alphabet previous value
                 in (value ++) `liftM` decode alphabet' value

    codeSize :: Alphabet -> Int
    codeSize (Lookup _ max) = min 12 $ floor (logBase 2 $ fromIntegral max) + 1

    initialAlphabet :: Alphabet
    initialAlphabet = Lookup [[fromIntegral x] | x <- [0 .. endOfInfoCode]]
                             (fromIntegral endOfInfoCode + 1)

    extendAlphabet :: Alphabet -> [Word8] -> [Word8] -> Alphabet
    extendAlphabet alphabet@(Lookup codes max) prev new =
      if null prev || max >= 4096 then alphabet
      else Lookup (codes ++ [prev ++ [head new]]) (max + 1)

    lookup :: Alphabet -> [Word8] -> Word16 -> [Word8]
    lookup (Lookup codes max) previous index =
      let index' = fromIntegral index :: Int
      in if index' < max then codes !! index'
         else previous ++ [head previous]

    -- source: http://graphics.stanford.edu/~seander/bithacks.html
    reverseBytes :: ByteString -> ByteString
    reverseBytes = B.map (\b ->
      fromIntegral $ (`shiftR` 32)
                   $ (* 0x0101010101)
                   $ (.&. 0x0884422110)
                   $ (* 0x80200802) (fromIntegral b :: Word64))
