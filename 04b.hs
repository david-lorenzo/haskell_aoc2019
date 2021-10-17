
low  = 248345
high = 746315

digits :: Int -> [Int]
digits n = go [] n
  where go acc m
          | m < 10    = m:acc
          | otherwise = go ((m `rem` 10):acc) (m `div` 10)

increasing (x:[])   = True
increasing (x:y:zs) = if x > y then False else increasing (y:zs)

repeating (x:[])   = False
repeating (x:y:zs) = if x == y then True else repeating (y:zs)

hasDouble (x:xs) = go (1, x) xs
  where
    go (state, prev) [x] =
      case (state, prev == x) of
        (1, True)  -> True
        (2, False) -> True
        (_,     _) -> False
    go (state, prev) (x:xs) =
      case (state, prev == x) of
        (2, False) -> True
        (1, True)  -> go (2, x) xs
        (2, True)  -> go (3, x) xs
        (i, True)  -> go (i+1, x) xs
        (_, False) -> go (1, x) xs


main = do
  let res2 = length . filter hasDouble . filter repeating . filter increasing . map digits $ [low..high]
  putStrLn . ("Part 2: " ++) . show $ res2

