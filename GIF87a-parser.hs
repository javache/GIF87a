import Control.Monad (forM_)
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Data.Either
import Data.List (nub)
import System.Environment

import GIF87a.Image
import GIF87a.Parser

-- | Runs the main program
main :: IO ()
main = do
  args <- getArgs
  forM_ args (\path -> do
    input <- B.readFile path
    case parseOnly parser input of
      Left err  -> putStrLn err
      Right img -> do print img
                      let x = pixels $ head $ lefts $ descriptors img
                      print (length x, nub $ map length x)
    )
