import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Set as S
import System.Environment

import Vec3
import qualified Vec3 as V3

data Moon = Moon {position :: Vec3 Int, velocity :: Vec3 Int} deriving (Eq, Ord)

instance Show Moon where
  show m = let pos = position m
               vel = velocity m
           in concat [
                      "pos=<x= ",
                      show $ x pos,
                      ", y= ",
                      show $ y pos,
                      ", z= ",
                      show $ z pos,
                      ">, vel=<x= ",
                      show $ x vel,
                      ", y= ",
                      show $ y vel,
                      ", z= ",
                      show $ z vel,
                      ">"
                     ]


list2Moon :: [Int] -> Moon
list2Moon [x, y, z] = Moon {position=Vec3 x y z, velocity=mempty}


line2Vec3 :: String -> Moon
line2Vec3 x = x                   &
              tail                & -- removing "<" from the start
              init                & -- removing ">" from the end
              splitOn ", "        & -- separating each postition component
              map (splitOn "=")   & -- splitting axis name from position value
              map (head . tail)   & -- dropping axis names
              map read            & -- converting to float values
              list2Moon


stepMoon :: Moon -> [Moon] -> Moon
stepMoon m ms = Moon (p + vel) vel
  where p   = position m
        ps  = map position $ filter (/= m) ms
        vel = velocity m + (sum $ map signum $ map (\x -> x - p) ps)


stepWorld :: [Moon] -> [Moon]
stepWorld ms = stepMoon <$> ms <*> [ms]


data Axis = Axis {
                   axisX :: [Int],
                   axisY :: [Int],
                   axisZ :: [Int]
                 } deriving (Eq, Show, Ord)


axis :: [Moon] -> Axis
axis ms = Axis mx my mz where
  mx = concat $ map (\m -> [x $ position m, x $ velocity m]) ms
  my = concat $ map (\m -> [y $ position m, y $ velocity m]) ms
  mz = concat $ map (\m -> [z $ position m, z $ velocity m]) ms


data CycleStates = CycleStates {
                                  cyclesX :: S.Set [Int],
                                  cyclesY :: S.Set [Int],
                                  cyclesZ :: S.Set [Int],
                                  cyclesFlags :: [Bool]
                               } deriving (Eq, Show, Ord)


emptyCycleState = CycleStates S.empty S.empty S.empty [False, False, False]


storeState :: CycleStates -> Axis -> CycleStates
storeState states axis = CycleStates newCX newCY newCZ newFlags where
  CycleStates {cyclesX=cx, cyclesY=cy, cyclesZ=cz, cyclesFlags=_} = states
  Axis {axisX=ax, axisY=ay, axisZ=az} = axis
  newFlags = [ax `S.member` cx, ay `S.member` cy, az `S.member` cz]
  newCX = S.insert ax cx
  newCY = S.insert ay cy
  newCZ = S.insert az cz


main = do
  -- args: inputfile number_iterations
  -- args: data/input12.txt
  inputFilePath:_ <- getArgs
  -- parsing input data
  inputData <- lines <$> readFile inputFilePath
  let moons = map line2Vec3 inputData
  let results0 = take 3  $ map axis $ iterate stepWorld moons
  -- calculating result
  let cycleInfo = moons                                     &
                  -- moving the moons step by step
                  iterate stepWorld                         &
                  -- extracting the values of the moons properties for
                  -- each of the axis
                  map axis                                  &
                  -- indexing each of the axis states in sets
                  scanl' storeState emptyCycleState         &
                  -- drop all the elements until the three axis
                  -- start to cycle their states
                  dropWhile (not . (all id) . cyclesFlags)  &
                  -- taking the first element
                  head
  -- each axis has its own cycle the solution will be the
  -- least common multiple of the number of states of each axis
  let solution = let sizex = S.size . cyclesX $ cycleInfo
                     sizey = S.size . cyclesY $ cycleInfo
                     sizez = S.size . cyclesZ $ cycleInfo
                 in foldl1 lcm [sizex, sizey, sizez]
  print solution
  return ()
