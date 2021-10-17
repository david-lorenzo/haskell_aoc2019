import Data.Bool
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad
import Control.Monad.ST (runST)
import System.Environment

run (noun, verb) v = runST (do
  m <- V.thaw v
  M.write m 1 noun
  M.write m 2 verb
  let loop = (\index  -> do
                opc   <- M.read m index
                addr1 <- M.read m (index+1)
                addr2 <- M.read m (index+2)
                addr3 <- M.read m (index+3)
                val1  <- M.read m addr1
                val2  <- M.read m addr2
                case opc of
                  1   -> M.write m addr3 (val1+val2) >> loop (index+4)
                  2   -> M.write m addr3 (val1*val2) >> loop (index+4)
                  99  -> return ())
  loop 0
  res   <- M.read m 0
  nounm <- M.read m 1
  verbm <- M.read m 2
  return (res, 100*nounm + verbm))


main = do
  inputFilePath <- head <$> getArgs
  program <- V.fromList . map read . splitOn "," . init <$> readFile inputFilePath
  putStrLn . ("Part 1: " ++) . show . fst . run (12, 2) $ program

