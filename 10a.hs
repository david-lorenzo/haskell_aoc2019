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


-- data Slope a = Angle a a | UpDir | DownDir | LeftDir | RightDir | Origin deriving (Show, Eq)
--
-- vec2ToSlope :: Integral a => Vec2 a -> Slope a
-- vec2ToSlope (Vec2 {x, y}) =
--   let angle = y % x
--       sx = signum x
--       sy = signum y
--   in case (x, sx, y, sy) of
--     (0,  _,  0, _) -> Origin
--     (0,  _, _,  1) -> DownDir
--     (0,  _, _, -1) -> UpDir
--     (_, -1, 0,  _) -> LeftDir
--     (_,  1, 0,  _) -> RightDir
--     (_, -1, _, -1) -> Angle (sx * denominator angle) (sy * numerator angle)
--     (_, sx, _, sy) -> Angle (denominator angle) (numerator angle)
--
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
  zipWith (\x s ->
    Spot Vec2 {x=x, y=y} s) [0..] )


-- slopes (Spot vo _) = map spotToSlope
--   where spotToSlope (Spot vd _) = vec2ToSlope (vd - vo)
--
-- slopes1 (Spot vo _) = map spotToSlope
--   where spotToSlope (Spot vd _) = (vd - vo)

slopes2 (Spot vo _) = map spotToSlope
  where spotToSlope (Spot vd _) = normVec2 (vd -vo)

asteroidsInSight :: Spot -> [Spot] -> Int
-- asteroidsInSight f asteroids = (+) (-1) . length . nub $ slopes2 f asteroids
asteroidsInSight f asteroids = (+) (-1) . length . S.fromList $ slopes2 f asteroids

zipFoldl1' f  xs ys = foldl1' f $ zip xs ys

maxSightings x y =
  let (n, _) = x
      (m, _) = y
  in if n > m then x else y

main = do
  inputFilePath <- head <$> getArgs
--   let inputFilePath = "data/input10demo.txt"
  inputData <- lines <$> readFile inputFilePath
  let inputMap = map (map charToSpotState) inputData
--   print $ length inputMap
--   print $ map length inputMap
  let ps = position inputMap
  let asteroids = filter (\(Spot _ x) -> x == Busy) . concat $ ps
--   print asteroids
--   print $ length asteroids
  let f = head asteroids
--   print $ group $ slopes f asteroids
--   mapM_ print $ zip (slopes f asteroids) asteroids
--   print ""
--   let num_groups = (+) (-1) . length . nub . slopes f $ asteroids
--   print num_groups
  let !sightings = asteroidsInSight <$> asteroids <*> [asteroids]
--   let s1 = Spot (Vec2 {x=2, y=2}) Busy
--   print $ slopes1 s1 asteroids
--   print ""
--   print $ slopes s1 asteroids
--   print . nub $ slopes s1 asteroids
--   mapM_ print $ nub . (slopes s1) $ asteroids
--   print sightings
  let !best = zipFoldl1' maxSightings sightings asteroids
  print best
  return ()
