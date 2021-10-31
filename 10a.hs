{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
import Data.Ratio
import Data.List
import Data.Set as S hiding (map, filter)
import System.Environment

data Vec2 a = Vec2 {x :: a, y :: a} deriving (Show, Eq)

instance Num a => Num (Vec2 a) where
  (+) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1+x2, y=y1+y2}
  (-) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1-x2, y=y1-y2}
  (*) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1*x2, y=y1*y2}
  abs (Vec2 {x, y}) = Vec2 {x=abs(x), y=abs(y)}
  fromInteger v = Vec2 {x=fromInteger v, y=0}
  signum (Vec2 {x, y}) = Vec2 {x=signum x * signum y, y=0}

instance Ord a => Ord (Vec2 a) where
  (<=) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) =
    if x1 == x2 then y1 <= y2
                else x1 <= x2

normVec2 :: Integral a => Vec2 a -> Vec2 a
normVec2 (Vec2 {x, y}) = let cd = gcd x y
  in case (x, y) of
        (0, 0) -> Vec2 0                0
        (0, _) -> Vec2 x                (y `div` abs y)
        (_, 0) -> Vec2 (x `div` abs x)  y
        _      -> Vec2 (x `div` cd)     (y `div` cd)


data SpotState = Empty | Busy deriving (Read, Eq)

instance Show SpotState where
  show a = spotStateToChar a : ""


data Spot = Spot (Vec2 Int) SpotState deriving (Eq)

instance Show Spot where
  show (Spot (Vec2 {x, y}) s) = show (x,y,s)


charToSpotState :: Char -> SpotState
charToSpotState '.' = Empty
charToSpotState '#' = Busy


spotStateToChar :: SpotState -> Char
spotStateToChar Empty = '.'
spotStateToChar Busy = '#'


position :: [[SpotState]] -> [[Spot]]
position = flip zipWith [0..] (\y ->
  flip zipWith [0..] (\x s -> Spot (Vec2 x y) s))


slopes (Spot vo _) = map spotToSlope
  where spotToSlope (Spot vd _) = normVec2 (vd -vo)


asteroidsInSight :: Spot -> [Spot] -> Int
asteroidsInSight f asteroids = (+) (-1) . length . S.fromList $ slopes f asteroids


zipFoldl1' f xs ys = foldl1' f $ zip xs ys


maxSightings x y =
  let (n, _) = x
      (m, _) = y
  in if n > m then x else y


main = do
  inputFilePath <- head <$> getArgs
--   let inputFilePath = "data/input10demo.txt"
  -- parsing input data
  inputData <- lines <$> readFile inputFilePath
  let inputMap = map (map charToSpotState) inputData

  -- locating positions for each spot
  let ps = position inputMap

  -- filtering only asteroids
  let asteroids = filter (\(Spot _ x) -> x == Busy) . concat $ ps

  -- for each asteroid find all directions where we can see other asteroids
  let !sightings = asteroidsInSight <$> asteroids <*> [asteroids]

  -- for all the results, find the spot with the maximum sightings
  let !best = zipFoldl1' maxSightings sightings asteroids
  print best
  return ()
