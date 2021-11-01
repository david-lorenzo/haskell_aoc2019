import Data.Function
import Data.List
import Data.List.Split
import System.Environment

import Vec3
import Vec3 as V3

data Moon = Moon {position :: Vec3 Float, velocity :: Vec3 Float} deriving (Eq, Ord)

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

data Energy = Energy { pot :: Float, kin :: Float, total :: Float } deriving (Eq, Ord)

instance Show Energy where
  show m = concat [
                    "pot: ",
                    show $ pot m,
                    "kin: ",
                    show $ kin m,
                    "total: ",
                    show $ total m
                  ]


list2Moon :: [Float] -> Moon
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


energy :: Moon -> Energy
energy m = Energy potencialE kineticE totalE
  where ener = sum . map abs . V3.toList
        potencialE = ener . position $ m
        kineticE   = ener . velocity $ m
        totalE     = potencialE * kineticE


systemEnergy :: [Moon] -> Float
systemEnergy = sum . map (total . energy)

main = do
  -- args: inputfile number_iterations
  -- args: data/input12.txt 1000
  inputFilePath:iterations:_ <- getArgs
  let n = (read iterations) :: Int
  -- parsing input data
  inputData <- lines <$> readFile inputFilePath
  let moons = map line2Vec3 inputData
  -- calculating result
  let results = iterate stepWorld moons !! n
  print . systemEnergy $ results
  return ()
