import Data.Maybe
import Data.List
import Data.List.Split
import System.Environment

type Point = (Int, Int)
type Segment = (Point, Point)

instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y)   = (negate x, negate y)
  fromInteger x   = (fromInteger x, 0)
  abs (x, y)      = (abs x, abs y)
  signum (x, y)   = (signum x, signum y)

parse :: String -> Point
parse (x:xs) = let d = read xs
  in case x of
        'R' -> ( d, 0)
        'L' -> (-d, 0)
        'U' -> (0,  d)
        'D' -> (0, -d)


segments :: [Point] -> [Segment]
segments (x:[]) = []
segments (x:y:zs) = (x, y) : segments (y:zs)


intersectPoint :: Segment -> Segment -> Maybe Point
intersectPoint ((ax, ay), (bx, by)) ((cx, cy), (dx, dy)) =
  let vertical1 = ax == bx
      vertical2 = cx == dx
      s1xlow = min ax bx
      s1xup  = max ax bx
      s1ylow = min ay by
      s1yup  = max ay by
      s2xlow = min cx dx
      s2xup  = max cx dx
      s2ylow = min cy dy
      s2yup  = max cy dy
      s2xcross = s1xlow <= cx && cx <= s1xup && s2ylow <= ay && ay <= s2yup
      s2ycross = s1ylow <= cy && cy <= s1yup && s2xlow <= ax && ax <= s2xup
  in if vertical1 == vertical2
     then Nothing
     else if vertical1
            then if s2ycross then Just (ax, cy) else Nothing
            else if s2xcross then Just (cx, ay) else Nothing

manhattan :: Point -> Point -> Int
manhattan p1 p2 = x + y where (x, y) = abs (p1 - p2)

seglength :: Segment -> Int
seglength (p1, p2) = manhattan p1 p2


insegment :: Point -> Segment -> (Bool, Int)
insegment (px, py) ((ax, ay), (bx, by)) =
  let sminy = min ay by
      smaxy = max ay by
      sminx = min ax bx
      smaxx = max ax bx
      vertical   = px == ax && px == bx && sminy <= py && py <= smaxy
      horizontal = py == ay && py == by && sminx <= px && px <= smaxx
  in case (vertical, horizontal) of
          (True, _)      -> (True,  seglength ((px, py), (ax, ay)))
          (_, True)      -> (True,  seglength ((px, py), (ax, ay)))
          (False, False) -> (False, seglength ((ax, ay), (bx, by)))

wireLength :: Point -> [Segment] -> Int
wireLength p wire = sumLengths distances
  where distances = map (insegment p) wire
        sumLengths ((True , d):_)    = d
        sumLengths ((False, d):rest) = d + sumLengths rest

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- map (map parse . splitOn ",") . lines <$> readFile inputFilePath
  let [w1, w2] = map segments . map (scanl (+) (0, 0)) $ inputLines
  let intersections = filter (/= (0, 0)) . catMaybes $ [intersectPoint s1 s2 | s1 <- w1, s2 <- w2]
  let minLength= minimum [wireLength p w1 + wireLength p w2 | p <- intersections]
  print minLength

