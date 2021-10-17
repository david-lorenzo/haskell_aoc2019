import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import System.Environment

mkOrbit [a, b] = (a, b)

objects = nub . go
  where go [] = []
        go ((a,b):xs) = a:b:(go xs)

buildDistanceMap orbits distances obj = go distances obj
  where go dm obj | M.member obj dm = dm
                  | otherwise       = let next = head [co | (co, oo) <- orbits, oo == obj]
                                          newDm = go dm next
                                          nextd = newDm M.! next
                                      in M.insert obj (nextd + 1) newDm

main = do
  inputFilePath <- head <$> getArgs
  orbits <- map mkOrbit . map (splitOn ")") . lines <$> readFile inputFilePath
  let xs = objects orbits
  let distances = foldl' (buildDistanceMap orbits) (M.singleton "COM" 0) xs
  let res = M.foldl (+) 0 distances
  putStrLn . ("Result: " ++) . show $ res

