-- import Data.Function
import Control.Monad
import Data.List
import Data.List.Split
import Data.IORef
import qualified Data.Map as M
import System.Environment

type Edge = ((String, String), (Int, Int))

parseElement :: String -> (String, Int)
parseElement desc = (name, quantity)
  where [q, name] = splitOn " " desc
        quantity = read q

parseLine :: String -> [Edge]
parseLine line = zipWith makeEdge lhsts $ repeat rhst
  where makeEdge (sa, na) (sb, nb) = ((sa, sb), (na, nb))
        [lhss, rhs] = splitOn " => " line
        rhst  = parseElement rhs
        lhsts = map parseElement . splitOn ", " $ lhss


react :: (Int, Int) -> Int -> Int
react (iq, oq) t = iq * case t `divMod` oq of
                        (n, 0) -> n
                        (n, _) -> n + 1


calc :: [Edge] -> Int
calc edges = go "ORE"
  where outedges x = filter ((==x) . fst . fst) edges
        go x = if x == "FUEL"
               then 1
               else sum $ map (\((start, end), v) ->
                  react v (go end)) (outedges x)

main = do
  inputFilePath:_ <- getArgs
  -- parsing input data
  inputData <- join . (map parseLine . lines) <$> readFile inputFilePath
  print $ calc inputData
