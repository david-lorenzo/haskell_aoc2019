{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
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

-- sqDistance :: Num a => Vec2 a -> Vec2 a -> a
-- sqDistance (Vec2 x1 y1) (Vec2 x2 y2) =
--   let xd = x1-x2
--       yd = y1-y2
--   in xd*xd + yd*yd

sqDistance :: Num a => Vec2 a -> Vec2 a -> a
sqDistance v1 v2 =
  let xd = x v1 - x v2
      yd = y v1 - y v2
  in xd*xd + yd*yd

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


spotPos :: Spot -> Vec2 Int
spotPos (Spot v _) = v

spotState :: Spot -> SpotState
spotState (Spot _ s) = s


position :: [[SpotState]] -> [[Spot]]
position = flip zipWith [0,(-1)..] (\y ->
  flip zipWith [0..] (\x s -> Spot (Vec2 x y) s))


vec2sToSlope  :: Vec2 Int -> Vec2 Int -> Vec2 Int
vec2sToSlope vo vd = normVec2 (vd - vo)

-- slopes :: Spot -> [Spot] -> [Vec2 Int]
-- slopes (Spot vo _) = map spotToSlope
--   where spotToSlope (Spot vd _) = normVec2 (vd -vo)

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


-- vec2Angle :: Vec2 Int -> Int
-- vec2Angle (Vec2 {x, y}) = 450 - angle3
--   where angle = (floor $ atan2 (fromIntegral y) (fromIntegral x) / pi * 180) `mod` 360
--         angle3 = if 0 <= angle && angle <= 90
--                  then angle + 360 else angle

-- vec2Angle :: Vec2 Int -> Int
-- vec2Angle (Vec2 {x, y}) = 450 - angle3
--   where angle = (atan2 (fromIntegral y) (fromIntegral x) / pi * 180)
--         angle2 = if angle < 360 then 360 + angle else angle
--         angle3 = if 0 <= angle2 && angle2 <= 90
--                  then angle2 + 360 else angle2

vec2Angle v = angle4 where
  angle = (atan2 (fromIntegral $ y v) (fromIntegral $ x v) / pi * 180)
  angle2 = if angle < 0 then 360 + angle else angle
  angle3 = 90 - angle2
  angle4 = if angle3 < 0
           then 360 + angle3
           else angle3


debugFlag = False
debug name values = if debugFlag
                    then debugi name values
                    else return ()

debugi name values = do
  print "\n--"
  print name
  mapM_ print values

main = do
  inputFilePath <- head <$> getArgs
--   let inputFilePath = "data/input10demo210.txt"
  -- parsing input data
  inputData <- lines <$> readFile inputFilePath
  let inputMap = map (map charToSpotState) inputData

  -- locating positions for each spot
  let ps = position inputMap

  -- filtering only asteroids
  let asteroids = filter (\x -> spotState x == Busy) . concat $ ps

  -- for each asteroid find all directions where we can see other asteroids
  let !sightings = asteroidsInSight <$> asteroids <*> [asteroids]

  -- for all the results, find the spot with the maximum sightings
  let !best = zipFoldl1' maxSightings sightings asteroids
  print best

  let bestSpot = snd best                   :: Spot
  let bestPos = spotPos bestSpot            :: Vec2 Int
  let dist = sqDistance bestPos . spotPos

  -- removing best spot
  let targets = filter (/= bestSpot) asteroids

  -- getting all the slopes again
  let routes = zip (slopes bestSpot targets) targets
  print ("len ", length routes)

  -- sort asteroids by slope
  let routes2 = sortBy (\a b -> compare (fst a) (fst b)) routes
  print ("len ", length routes2)
  print "up:"
  let up = filter (\(_, (a,_)) -> a == Vec2 0 1) $ zip [0..] routes2
  mapM_ print $ up

  -- group by slope
  let routes3 = groupBy (\a b -> fst a == fst b) routes2
  debug "routes3" routes3
  let up3 = filter (\(_, a) -> (fst . head $ a) == Vec2 0 1) $ zip [0..] routes3
  debug "routes3 1" up3
  debug "routes3 2" $ snd . head $ up3

  -- removing slope redundancy: [(Vec2, Spot)] -> [(Vec2, [Spot])]
  let routes3_1 = map (\x -> (fst . head $ x, map snd x)) routes3
  debug "routes3_1" routes3_1
  let b = filter (\(a,c) -> (Spot (Vec2 11 (-12)) Busy) `elem` c) routes3_1
  print ("b", b)

--
--   let routes3_2 = map (\(a, b) -> (a, map (dist . spotPos) b)) routes3_1
--   let routes3_2 = map (\(a, b) -> (a, map dist b)) routes3_1
--   print "--"
--   print "routes3_2"
--   mapM_ print routes3_2
--   let routes4 = [1]
  -- ordering groups by distance
  let orderByDistance = \(a, b) -> (a, sortBy (\x y -> compare (dist x) (dist y)) b)
  let routes4 = map orderByDistance routes3_1
--   debug "routes4" routes4
  print $ filter ((==Vec2 0 1) . fst) routes4

--   let routes4_1 = map (\(a, b) -> (a, map (dist . spotPos) b)) routes4
--   let routes4_1 = map (\(a, b) -> (a, map dist b)) routes4
--   print "--"
--   print "routes4_1"
--   mapM_ print routes4_1
   -- order groups by distance
--   let routes4 = map (sortBy (\x y -> compare (dist . spotPos . snd $ x) (dist . spotPos. snd $ y))) routes3
--   print "routes4"
--   mapM_ print routes4
--   print "--"
--   print "--"
  mapM_ print $ [ ("Right 0", vec2Angle (Vec2 1 0)),
                  ("Up 90", vec2Angle (Vec2 0 1)),
                  ("Left 180", vec2Angle (Vec2 (-1) 0)),
                  ("Down 270", vec2Angle (Vec2 0 (-1))) ]
--   print $ vec2Angle $ fst $ head routes5
  -- calculating the angles
  let  routes6 = map (\(a, b) -> (a, vec2Angle a, b)) routes4
--   debug "routes6" routes6
  print $ filter (\(a, _,_) -> a ==Vec2 0 1) routes6

--   let routes6 = map (\(a, b) -> (vec2Angle a, b)) routes5
--   let routes7 = sortBy (\a b-> compare (fst a) (fst b)) routes6
--   print "--"
--   mapM_ print routes7
  -- ordering groups by angle
  let getAngle = \(a, b, c) -> b
  let routes7 = sortBy (\a b -> compare (getAngle a) (getAngle b)) routes6
--   debug "routes7" routes7

  print $ head routes7

--   debugi "routes7" $ take 2 routes7
--   let up = filter (\(a, b, c) -> a == Vec2 0 1) routes7
--   print up

  -- once everything is sorted we get the asteroids groups
  let getSpot = \(_, _, a) -> a
  let routes8 = map getSpot routes7
  debug "routes8" routes8

  -- flattening the structure, closer rings first, further rings latter
  let !routes9 = concat $ transpose routes8
  debug "routes9" routes9

--   let result = (take 1) . (drop 199) $ routes9
--   print result
  print ("1", routes9 !! 0)
  print ("2", routes9 !! 1)
  print ("3", routes9 !! 2)
  print ("10", routes9 !! 9)
  print ("20", routes9 !! 19)
  print ("50", routes9 !! 49)
  print ("100", routes9 !! 99)
  print ("199", routes9 !! 198)
  print ("200", routes9 !! 199)
  print ("201", routes9 !! 200)
  print ("299", routes9 !! 298)

  let betAsteroid = routes9 !! 199
  let Vec2 {x=a, y=b} = spotPos $ betAsteroid
  let result = 100 * a + abs b
  print result

  return ()


