module Intcode7b (run) where

import Data.IORef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

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

run :: (V.Vector Int, Int) -> IO (Int, V.Vector Int, Bool)
run (v, pc) user = do
  m <- V.thaw v
  userRef <- newIORef user
  outputRef <- newIORef [0]
  haltedRef <- newIORef False
  let len = M.length m

  let userInput = (do ui <- readIORef userRef
                      let (uih:uit) = ui
                      writeIORef userRef uit
                      return uih)

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
                case instr of
                  READ  -> do p1 <- M.read m (index+1)
                              uid <- userInput
                              M.write m p1 uid
                              loop (index+2)
                  PRINT -> do p1 <- M.read m (index+1)
                              value <- M.read m p1
                              modifyIORef outputRef (\oref -> value:oref)
                              return (index+2, False)
--                              loop (index+2)
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
                  HALT  -> do writeIORef haltedRef True
                              return (index+1, True))
  (pc', halt) <- loop pc
  output <- readIORef outputRef
  memory <- V.freeze m
  halted <- readIORef haltedRef)
  return (head output, memory, halted)

