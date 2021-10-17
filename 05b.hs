import System.IO
import System.Environment
import Data.Bool
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad
import Control.Monad.ST (runST)

data OP = SUM
        | MUL
        | READ
        | PRINT
        | JTRUE
        | JFALSE
        | LESST
        | EQUALS
        | HALT deriving (Show, Eq)

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
                    5  -> JTRUE
                    6  -> JFALSE
                    7  -> LESST
                    8  -> EQUALS
                    99 -> HALT

run :: Int -> V.Vector Int -> IO (V.Vector Int)
run user v = do
  m <- V.thaw v
  let len = M.length m

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
                let (modes, instr) = decode opc
                let (a, b, c) = modes
--                forM_ [0..(len-1)] (\i -> do v <- M.read m i
--                                             putStr . (" " ++) . show $ v)
--                putStr " | "
--                print (index, modes, instr)
                case instr of
                  READ  -> do p1 <- M.read m (index+1)
                              opRead p1 
                              loop (index+2)
                  PRINT -> do p1 <- M.read m (index+1)
                              value <- opPrint p1
                              print value
                              loop (index+2)
                  SUM   -> do p1 <- M.read m (index+1)
                              p2 <- M.read m (index+2)
                              p3 <- M.read m (index+3)
                              runOp modes (+) p1 p2 p3
                              loop (index+4)
                  MUL   -> do p1 <- M.read m (index+1)
                              p2 <- M.read m (index+2)
                              p3 <- M.read m (index+3)
                              runOp modes (*) p1 p2 p3
                              loop (index+4)
                  JTRUE -> do p1 <- M.read m (index+1)
                              p2 <- M.read m (index+2)
                              v1 <- if c == 1 then return p1 else M.read m p1
                              v2 <- if b == 1 then return p2 else M.read m p2
                              let nextInstruction = if v1 /= 0 then v2 else (index+3)
                              loop nextInstruction
                  JFALSE -> do p1 <- M.read m (index+1)
                               p2 <- M.read m (index+2)
                               v1 <- if c == 1 then return p1 else M.read m p1
                               v2 <- if b == 1 then return p2 else M.read m p2
                               let nextInstruction = if v1 == 0 then v2 else (index+3)
                               loop nextInstruction
                  LESST -> do p1 <- M.read m (index+1)
                              p2 <- M.read m (index+2)
                              p3 <- M.read m (index+3)
                              v1 <- if c == 1 then return p1 else M.read m p1
                              v2 <- if b == 1 then return p2 else M.read m p2
                              let res = if v1 < v2 then 1 else 0
                              M.write m p3 res
                              loop (index+4)
                  EQUALS -> do p1 <- M.read m (index+1)
                               p2 <- M.read m (index+2)
                               p3 <- M.read m (index+3)
                               v1 <- if c == 1 then return p1 else M.read m p1
                               v2 <- if b == 1 then return p2 else M.read m p2
                               let res = if v1 == v2 then 1 else 0
                               M.write m p3 res
                               loop (index+4)
                  HALT  -> return ())
  loop 0
  V.freeze m


integer :: String -> Int
integer = read

readInput = map integer

main = do
  -- arguments "5 input5.txt"
  [inputData, inputFilePath] <- getArgs
  program <- V.fromList . readInput . splitOn "," . init <$> readFile inputFilePath
  res <- run (integer inputData) program
  putStrLn ""


