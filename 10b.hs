{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
import Data.Function ((&))
import Data.Ratio
import Data.List
import Data.Set as S hiding (map, filter, take, drop)
import System.Environment

data Vec2 a = Vec2 {x :: a, y :: a} deriving (Show, Eq)

instance Num a => Num (Vec2 a) where
  (+) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1+x2, y=y1+y2}
  (-) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1-x2, y=y1-y2}
  (*) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) = Vec2 {x=x1*x2, y=y1*y2}
  abs (Vec2 {x, y}) = Vec2 {x=abs(x), y=abs(y)}
  fromInteger v = Vec2 {x=fromInteger v, y=0}
  signum (Vec2 {x, y}) = Vec2 {x=signum x * signum y, y=0}

instance Integral a => Ord (Vec2 a) where
  (<=) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) =
    case (x1==x2, y1==y2) of
      (True, True) -> (x1*x1 + y1*y1) <= (x2*x2 + y2*y2)
      (True, _   ) -> y1 <= y2
      (_   , _   ) -> x1 <= x2


sqDistance :: Num a => Vec2 a -> Vec2 a -> a
sqDistance v1 v2 =
  let xd = x v1 - x v2
      yd = y v1 - y v2
  in xd*xd + yd*yd


normVec2 :: Integral a => Vec2 a -> Vec2 a
normVec2 (Vec2 {x, y}) =
  let cd = gcd x y
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


spotPos :: Spot -> Vec2 Int
spotPos (Spot v _) = v

spotState :: Spot -> SpotState
spotState (Spot _ s) = s


-- y axis is flipped
position :: [[SpotState]] -> [[Spot]]
position = flip zipWith [0,(-1)..] (\y ->
  flip zipWith [0..] (\x s -> Spot (Vec2 x y) s))


vec2sToSlope  :: Vec2 Int -> Vec2 Int -> Vec2 Int
vec2sToSlope vo vd = normVec2 (vd - vo)


slopes :: Spot -> [Spot] -> [Vec2 Int]
slopes so ss = map (vec2sToSlope p0) ps
  where p0 = spotPos so       -- reference position
        ps = map spotPos ss   -- target positions


asteroidsInSight :: Spot -> [Spot] -> Int
asteroidsInSight f asteroids = (+) (-1) . length . S.fromList $ slopes f asteroids


zipFoldl1' f xs ys = foldl1' f $ zip xs ys


maxSightings x y =
  let (n, _) = x
      (m, _) = y
  in if n > m then x else y


vec2Angle v = angle4 where
  angle = (atan2 (fromIntegral $ y v) (fromIntegral $ x v) / pi * 180)
  angle2 = if angle < 0 then 360 + angle else angle
  angle3 = 90 - angle2
  angle4 = if angle3 < 0
           then 360 + angle3
           else angle3

--   mapM_ print $ [ ("Right 0", vec2Angle (Vec2 1 0)),
--                   ("Up 90", vec2Angle (Vec2 0 1)),
--                   ("Left 180", vec2Angle (Vec2 (-1) 0)),
--                   ("Down 270", vec2Angle (Vec2 0 (-1))) ]


main = do
  -- reading input and parsing input data
  inputFilePath <- head <$> getArgs
  inputData <- lines <$> readFile inputFilePath
  let inputMap = map (map charToSpotState) inputData

  -- locating positions for each spot
  let ps = position inputMap

  -- filtering only asteroids
  let asteroids = filter ((==Busy) . spotState) $ concat ps

  -- for each asteroid find all directions where we can see other asteroids
  let sightings = asteroidsInSight <$> asteroids <*> [asteroids]

  -- for all the results, find the spot with the maximum sightings
  let best = zipFoldl1' maxSightings sightings asteroids
  print best
  let bestSpot = snd best                   :: Spot
  let bestPos = spotPos bestSpot            :: Vec2 Int

  -- auxiliary functions
  let dist = sqDistance bestPos . spotPos   :: Spot -> Int
  let orderByDistance = \(a, b) -> (a, sortBy (\x y -> compare (dist x) (dist y)) b)
  let getAngle = \(a, b, c) -> b
  let getSpot = \(_, _, a) -> a

  let destructionSequence = asteroids                                 &
                -- removing best spot from the list
                filter (/= bestSpot)                                  &
                -- getting all the slopes again :: [(Vec2 Int, Spot)]
                \x -> zip (slopes bestSpot x) x                       &
                -- sort asteroids by slope
                sortBy (\a b -> compare (fst a) (fst b))              &
                -- group by slope
                groupBy (\a b -> fst a == fst b)                      &
                -- removing slope redundancy: [(Vec2, Spot)] -> [(Vec2, [Spot])]
                map (\x -> (fst . head $ x, map snd x))               &
                -- order groups of asteroids by distance
                map orderByDistance                                   &
                -- calculating groups angles before ordering
                map (\(a, b) -> (a, vec2Angle a, b))                  &
                -- ordering groups clockwise
                sortBy (\a b -> compare (getAngle a) (getAngle b))    &
                -- once everything is sorted we get the asteroids groups
                map getSpot                                           &
                -- flattening the structure, closer rings first, further rings latter
                transpose & concat

--   print ("1", destructionSequence !! 0)
--   print ("2", destructionSequence !! 1)
--   print ("3", destructionSequence !! 2)
--   print ("10", destructionSequence !! 9)
--   print ("20", destructionSequence !! 19)
--   print ("50", destructionSequence !! 49)
--   print ("100", destructionSequence !! 99)
--   print ("199", destructionSequence !! 198)
--   print ("200", destructionSequence !! 199)
--   print ("201", destructionSequence !! 200)
--   print ("299", destructionSequence !! 298)

  let betAsteroid = destructionSequence !! 199
  let Vec2 {x=a, y=b} = spotPos $ betAsteroid
  let result = 100 * a + abs b
  print result

  return ()


