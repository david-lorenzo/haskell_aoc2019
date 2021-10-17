import System.Environment
import Data.List
import Data.List.Split

mkOrbit [a, b] = (a, b)

path orbits obj n = let next = head [x | (x, y) <- orbits, y == obj]
                    in case obj of
                          "COM" -> [("COM", n)]
                          _     -> (obj, n):(path orbits next (n+1))

main = do
  inputFilePath <- head <$> getArgs
  orbits <- map mkOrbit . map (splitOn ")") . lines <$> readFile inputFilePath
  let res = head [ lx + ly - 2 | (x, lx) <- path orbits "YOU" 0
                               , (y, ly) <- path orbits "SAN" 0
                               , x == y ]
  putStrLn . ("Result: " ++) . show $ res

