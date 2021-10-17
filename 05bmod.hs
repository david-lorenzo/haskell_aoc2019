import System.Environment
import qualified Data.Vector.Unboxed as V
import Data.List.Split
import Intcode (run)

integer :: String -> Int
integer = read

readInput = map integer

main = do
  -- arguments "5 input5.txt"
  [inputData, inputFilePath] <- getArgs
  program <- V.fromList . readInput . splitOn "," . init <$> readFile inputFilePath
  res <- run (integer inputData) program
  putStrLn ""


