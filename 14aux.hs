import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import System.Environment

data Product = Product Int String deriving (Show, Eq)

prodQ :: Product -> Int
prodQ (Product q _) = q

prodName :: Product -> String
prodName (Product _ name) = name

instance Ord Product where
  compare a b = compare (prodName a) (prodName b)

data Reaction = Reaction [Product] Product deriving (Show, Eq)

react :: Reaction -> Product
react (Reaction _ p) = p


inputs :: Reaction -> [Product]
inputs (Reaction ps _) = ps


parseProduct :: String -> Product
parseProduct desc = Product quantity name
  where [q, name] = splitOn " " desc
        quantity = read q


parseInput :: String -> Reaction
parseInput reaction = Reaction inputs product
  where [ins, out] = splitOn " => " reaction
        product = parseProduct out
        inputs = map parseProduct . splitOn ", " $ ins


printPair (Product iq iname) (Product oq oname) = do
  putStrLn . join $ ["\t", iname, " -> ", oname, " [label=\"", show iq, ":", show oq ,"\"];"]

printReaction (Reaction ps p) = do
  mapM_ (`printPair` p) ps


main = do
  inputFilePath:_ <- getArgs
--   let inputFilePath = "data/input14.txt"
  -- parsing input data
  inputData <- (map parseInput . lines) <$> readFile inputFilePath
  putStrLn "digraph {"
  mapM_ printReaction inputData
  putStrLn "}"


