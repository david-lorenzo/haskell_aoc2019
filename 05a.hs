import System.Environment
import System.IO
import Data.Bool
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad
import Control.Monad.ST (runST)

data OP = SUM | MUL | READ | PRINT | HALT deriving (Show, Eq)

decode :: Int -> ((Int, Int, Int), OP)
decode n = ((a, b, c), opc)
  where (a, rA) = ( n `div` 10000,  n `rem` 10000)
        (b, rB) = (rA `div`  1000, rA `rem`  1000)
        (c, op) = (rB `div`   100, rB `rem`   100)
        opc     = case op of
                    1  -> SUM
                    2  -> MUL
                    3  -> READ
                    4  -> PRINT
                    99 -> HALT

run :: Int -> V.Vector Int -> IO (V.Vector Int)
run user v = do
  m <- V.thaw v

  let opRead  = (\addr -> M.write m addr user >> return ()) :: Int -> IO ()
  let opPrint = (\addr -> M.read m addr) :: Int -> IO Int
  let runOp   = (\(a, b, c) op op1 op2 addr -> do
                    v1 <- if c == 1 then return op1 else M.read m op1
                    v2 <- if b == 1 then return op2 else M.read m op2
                    let res = op v1 v2
                    M.write m addr res
                    return ()) :: (Int, Int, Int) -> (Int -> Int -> Int) -> Int -> Int -> Int -> IO ()

  let loop = (\index  -> do
                opc <- M.read m index
                p1  <- M.read m (index+1)
                p2  <- M.read m (index+2)
                p3  <- M.read m (index+3)
                let (modes, instr) = decode opc
--                print (index, opc, modes, instr, p1, p2, p3)
--                hFlush stdout
                case instr of
                  READ  -> opRead p1  >> loop (index+2)
                  PRINT -> do value <- opPrint p1
                              print value
                              loop (index+2)
                  SUM   -> runOp modes (+) p1 p2 p3 >> loop (index+4)
                  MUL   -> runOp modes (*) p1 p2 p3 >> loop (index+4)
                  HALT  -> return ())
  loop 0
  V.freeze m

readInput :: [String] -> [Int]
readInput = map read


main = do
  inputFilePath <- head <$> getArgs
  program <- V.fromList . readInput . splitOn "," . init <$> readFile inputFilePath
--  print program
  res <- run 1 program
  putStrLn ""
--  putStrLn . ("Result: " ++) . show $ res


