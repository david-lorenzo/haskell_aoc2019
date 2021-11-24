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


calc :: [Edge] -> Int -> Int
calc edges target = go "ORE"
  where outedges x = filter ((==x) . fst . fst) edges
        go x = if x == "FUEL"
               then target
               else sum $ map (\((start, end), v) ->
                  react v (go end)) (outedges x)


binSearch f low high = let mid = low + (high - low) `div` 2
                           tooMuch = oreAvailable < f mid
                       in if low > high
                          then high
                          else if tooMuch
                                then binSearch f low (mid-1)
                                else binSearch f (mid+1) high

oreAvailable = 1000000000000

main = do
  inputFilePath:_ <- getArgs

  -- parsing input data
  inputData <- join . (map parseLine . lines) <$> readFile inputFilePath

  -- 14a solution
  let f = calc inputData
  let res = f 1
  print res

  -- a reasonable guess for the max Fuel lower bound
  let totalFuel = fst $ oreAvailable `divMod` res
  print ("totalFuel", totalFuel)

  -- as f is monotonic increasing we can binary search the solution
  -- we estimate the upper bound as twice the lower bound guessed
  let maxFuel = binSearch f totalFuel (2*totalFuel)

  -- printing solution
  let maxFuelOre = f maxFuel
  print ("maxFuel", maxFuel, "requires", maxFuelOre,
        "leftovers", oreAvailable - maxFuelOre)

  -- checking we indeed can't get more fuel with the ore available
  let doubleCheck = f (maxFuel + 1)
  print ("maxFuel+1", maxFuel+1, "requires", doubleCheck,
         "leftovers", oreAvailable - doubleCheck)
