import Control.Monad
import Data.Char
import Data.Maybe
-- import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import System.Environment

max_col = 81
max_row = 81


parseLine = zip [1..]

parse ss = zipWith (\x ys -> map (\(a, b) -> ((a, x), b)) ys) [1..] $ map parseLine ss

printBoard board = forM_ [1..max_row] (\row -> do
                    forM_ [1..max_col] (\col -> do
                      let ch = case M.lookup (col, row) board of
                                Just x -> x
                                Nothing -> '#'
                      putChar ch)
                    putChar '\n'
                    )

nowhere (c, r) board = value || neighbors
  where value = (/= '.') . fromJust $ M.lookup (c, r) board
        neighbors = (> 1) . length $ filter isJust [M.lookup (col, row) board |
                                     (col, row) <- [(c-1, r), (c+1, r), (c, r-1), (c, r+1)]]

-- clean board = head . (drop 16) $ cleanSeq --fst . head . dropWhile (\(x, y) -> x /= y) $ zip cleanSeq (drop 1 cleanSeq)
clean board = fst . head . dropWhile (\(x, y) -> x /= y) $ zip cleanSeq (drop 1 cleanSeq)
  where step b = M.filterWithKey (\k _ -> nowhere k b) b
        cleanSeq = iterate step board

findNodes board = M.filterWithKey f board
  where f k v = (v /= '.') || ((>2) . length $ filter isJust $ neigbors k)
        neigbors (c, r) = [M.lookup k1 board | k1 <- [(c-1, r), (c+1, r), (c, r-1), (c, r+1)]]

simplify board nodes = undefined

genEdges edges = M.foldMapWithKey f edges
  where f (c, r) idx1 = concat [edge idx1 (M.lookup k edges) | k <- [(c-1, r), (c+1, r), (c, r-1), (c, r+1)]]
        edge idx1 b = case b of
                        Just idx2 -> if idx1 < idx2 then concat ["\t", show idx1, "--", show idx2, ";\n"] else ""
                        Nothing -> ""

genNodes nodes edges = concat $ zipWith f idx labels
  where keys = map fst $ M.toList edges
        idx  = map (\k -> show $ fromJust $ (M.lookup k edges)) keys
        labels = map (\k -> show $ fromJust $ (M.lookup k nodes)) keys
        f index label = if label /= "'.'" then concat ["\t", index, " [label=\"", label, "\"]\n"] else ""

genDot nodes edges = concat ["graph {\n", genNodes nodes edges, genEdges edges, "}\n"]

main = do
  inputFilePath:_ <- getArgs
  inputData <- filter ((/= '#'). snd) . concat . parse . lines <$> readFile inputFilePath
  let board = clean $ M.fromList inputData
--   printBoard board
--   let nodes = M.fromList $ zip (map fst . M.toList $ board) [1..]
--   putStrLn $ genDot board nodes
  let nodes = findNodes board
  print nodes
  print (length nodes)
  simplify board nodes
--   let doors = filter (isUpper . snd) inputData
--   let keys = filter (isLower . snd) inputData
--   let player = filter ((== '@') . snd) inputData
--   let alleys = filter ((== '.') . snd) inputData
--   let objects = S.fromList $ concat [doors, keys, player]
--   let allElems = S.fromList inputData
--   print $ allElems S.\\ objects
--   print $ filter ((/= '.'). snd) inputData
  return ()
