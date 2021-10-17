import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Intcode (run)

integer :: String -> Int
integer = read

readInput = map integer

controlThrusters :: V.Vector Int -> Int -> [Int] -> IO Int
controlThrusters program isignal [] = return isignal
controlThrusters program isignal (p:ps) = do
  osignal <- run [p,  isignal] program
  controlThrusters program osignal ps

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  program <- V.fromList . readInput . splitOn "," . init <$> readFile inputFilePath
  osignals <- maximum <$> mapM (controlThrusters program 0) (permutations [0,1,2,3,4])
  print osignals
