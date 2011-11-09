module GIF87a.LZW (encodeLZW, decodeLZW) where

import Control.Applicative ((<$>))
import Control.Monad (liftM, foldM, foldM_)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Binary.Put hiding (putByteString)
import Data.Binary.BitPut
import Data.Binary.Strict.BitGet
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import Data.Word (Word8, Word16, Word64)

import Debug.Trace

-- | * Encode and decode

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
    initDecode = decode (initialDAlphabet clearCode) []

    decode :: DAlphabet -> [Word8] -> BitGet [Word8]
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
                  let new       = lookupDAlphabet alphabet previous code
                      alphabet' = extendDAlphabet alphabet previous new
                  in (new ++) `liftM` decode alphabet' new

-- | Encode a list of bytes in a bytestring using LZW
encodeLZW :: Int -> [Word8] -> BL.ByteString
encodeLZW initialCodeSize input = lazyReverseBytes $ runBitPut $ do
  -- TODO: what if max dictionary size is reached?
  let alphabet = initialEAlphabet clearCode
  putBits alphabet clearCode
  (alphabet', last) <- foldM encodeWord (alphabet, []) input
  putBits alphabet' $ fromJust $ lookupEAlphabet alphabet' last
  putBits alphabet' endOfInfoCode
  where
    clearCode = fromIntegral $ 2 ^ initialCodeSize :: Word16
    endOfInfoCode = clearCode + 1 :: Word16

    encodeWord :: (EAlphabet, [Word8]) -> Word8 -> BitPutM (EAlphabet, [Word8])
    encodeWord (alphabet, current) next =
      let new = current ++ [next]
      in case lookupEAlphabet alphabet new of
        Just code -> return (alphabet, new)
        Nothing   -> do
          let alphabet' = extendEAlphabet alphabet new
          putBits alphabet $ fromJust $ lookupEAlphabet alphabet' current
          return (alphabet', [next])

    putBits :: EAlphabet -> Word16 -> BitPut
    putBits alphabet code = do
      let bytes = runPut $ putWord16le code
      foldM_ (\n word -> do
          let bits = min 8 n
              word' = reverseByte word `shiftR` (fromIntegral $ 8 - bits)
          putNBits bits word'
          return $ n - bits
        ) (codeSize alphabet) (BL.unpack bytes)

-- * Alphabet
-- ** General alphabet methods
class Alphabet a where
  maxKey :: a -> Word16

codeSize :: Alphabet a => a -> Int
codeSize a = min 12 $ floor (logBase 2 $ fromIntegral $ maxKey a) + 1

-- ** Decoder alphabet methods
data DAlphabet = DAlphabet (Map Word16 [Word8]) Word16
                 deriving (Show)
instance Alphabet DAlphabet where
  maxKey (DAlphabet _ max) = max

initialDAlphabet :: Word16 -> DAlphabet
initialDAlphabet a =
  let values = [(x, [fromIntegral x]) | x <- [0 .. a - 1]]
  in DAlphabet (M.fromList values) (a + 2)

extendDAlphabet :: DAlphabet -> [Word8] -> [Word8] -> DAlphabet
extendDAlphabet alphabet@(DAlphabet codes max) prev new =
  if null prev || max >= 4096 then alphabet
  else DAlphabet (M.insert max (prev ++ [head new]) codes) (max + 1)

lookupDAlphabet :: DAlphabet -> [Word8] -> Word16 -> [Word8]
lookupDAlphabet (DAlphabet codes max) previous index =
  fromMaybe (previous ++ [head previous])
            (M.lookup (fromIntegral index) codes)

-- ** Encoder alphabet methods
data EAlphabet = EAlphabet (Map [Word8] Word16) Word16
                 deriving (Show)
instance Alphabet EAlphabet where
  maxKey (EAlphabet _ max) = max

initialEAlphabet :: Word16 -> EAlphabet
initialEAlphabet a =
  let values = [([fromIntegral x], x) | x <- [0 .. a - 1]]
  in EAlphabet (M.fromList values) (a + 1)

extendEAlphabet :: EAlphabet -> [Word8] -> EAlphabet
extendEAlphabet alphabet@(EAlphabet codes max) word =
  if max >= 4096 then alphabet
  else EAlphabet (M.insert word (max + 1) codes) (max + 1)

lookupEAlphabet :: EAlphabet -> [Word8] -> Maybe Word16
lookupEAlphabet (EAlphabet codes max) word = M.lookup word codes

-- * Utilities
-- | Reverse the order of bits of each byte in the bytestring
reverseBytes :: ByteString -> ByteString
reverseBytes = B.map reverseByte

lazyReverseBytes :: BL.ByteString -> BL.ByteString
lazyReverseBytes = BL.map reverseByte

-- source: http://graphics.stanford.edu/~seander/bithacks.html
reverseByte :: Word8 -> Word8
reverseByte b = fromIntegral
              $ (`shiftR` 32)
              $ (* 0x0101010101)
              $ (.&. 0x0884422110)
              $ (* 0x80200802) (fromIntegral b :: Word64)
