import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Intcode7b (run)

integer :: String -> Int
integer = read

readInput = map integer

run :: [Int] -> V.Vector Int -> IO (Int, V.Vector Int, Bool)
run user v = do

controlThrusters [va, vb, vc, vd, ve] [pa, pb, pc, pd, pe] isignal =
  (oa, va', ha) <- run [pa, isignal] va
  (ob, vb', hb) <- run [pb, isignal] vb
  (oc, vc', hc) <- run [pc, isignal] vc
  (od, vd', hd) <- run [pd, isignal] vd
  (oe, ve', he) <- run [pe, isignal] ve
  controlThrusters [va', vb', vc', vd', ve'] [pa, pb, pc, pd, pe] oe


main = do
  inputFilePath <- head <$> getArgs
  inputLines <- lines <$> readFile inputFilePath
  program <- V.fromList . readInput . splitOn "," . init <$> readFile inputFilePath
  osignals <- maximum <$> mapM (controlThrusters program 0) (permutations [0,1,2,3,4])
  print osignals
