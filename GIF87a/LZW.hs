module GIF87a.LZW (encodeLZW, decodeLZW) where

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
data DecoderState = Continue | Reset | Stop deriving (Show)

decodeLZW :: Int -> ByteString -> [Word8]
decodeLZW initialCodeSize input =
  case runBitGet (reverseBytes input) initDecode of
    Left msg -> error msg
    Right x  -> x
  where
    clearCode = fromIntegral $ 2 ^ initialCodeSize :: Word16
    endOfInfoCode = clearCode + 1 :: Word16

    initDecode :: BitGet [Word8]
    initDecode = decode initialAlphabet []

    decode :: Alphabet -> [Word8] -> BitGet [Word8]
    decode alphabet prevBytes = do
      code <- getLeftByteString $ codeSize alphabet
      let (bytes, state) = evalCode code alphabet prevBytes
      trace (show (bytes, state)) $ case state of
        Continue -> let alphabet' = extendAlphabet alphabet prevBytes bytes
                    in decode alphabet' bytes
                       >>= \rest -> return (bytes ++ rest)
        Reset    -> initDecode
        Stop     -> return []

    evalCode :: ByteString -> Alphabet -> [Word8] -> ([Word8], DecoderState)
    evalCode bytes alphabet previous
      | code == clearCode     = ([], Reset)
      | code == endOfInfoCode = ([], Stop)
      | otherwise             = (lookup alphabet previous code, Continue)
      where reversed = B.unpack $ reverseBytes bytes :: [Word8]
            code = case reversed of
                    [x]   -> fromIntegral x :: Word16
                    [x,y] -> (fromIntegral x `shiftL` 8) .|. fromIntegral y

    codeSize :: Alphabet -> Int
    codeSize (Lookup _ max) = floor (logBase 2 $ fromIntegral max) + 1

    initialAlphabet :: Alphabet
    initialAlphabet = Lookup [return x | x <- [0 .. fromIntegral endOfInfoCode]]
                             (fromIntegral endOfInfoCode)

    extendAlphabet :: Alphabet -> [Word8] -> [Word8] -> Alphabet
    extendAlphabet alphabet@(Lookup codes max) prev new =
      if null prev then alphabet
      else trace ("Adding " ++ show [prev ++ [head new]] ++ " to dictionary") $
           Lookup (codes ++ [prev ++ [head new]]) (max + 1)

    lookup :: Alphabet -> [Word8] -> Word16 -> [Word8]
    lookup (Lookup codes max) previous index =
      let index' = fromIntegral index :: Int
      in if index' <= max then codes !! index'
         else previous ++ [head previous]

    -- source: http://graphics.stanford.edu/~seander/bithacks.html
    reverseBytes :: ByteString -> ByteString
    reverseBytes = B.map (\b ->
      fromIntegral $ (`shiftR` 32)
                   $ (* 0x0101010101)
                   $ (.&. 0x0884422110)
                   $ (* 0x80200802) (fromIntegral b :: Word64))
