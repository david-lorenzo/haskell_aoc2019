import Data.IORef
import Control.Monad

test :: [Int] -> IO Int
test xs = do
  ys <- newIORef xs
  let modify = (do 
                       lys <- readIORef ys
                       writeIORef ys (tail lys)
                       return (head lys))
  forM_ [0..3] (\i -> do a <- modify
                         print (i, a))
  return 0

main = do
  r <- test [4..7]
  print r

