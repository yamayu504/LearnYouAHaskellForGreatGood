
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunc [] . words 
    where foldingFunc (x:y:ys) "*" = (y * x):ys
          foldingFunc (x:y:ys) "+" = (y + x):ys
          foldingFunc (x:y:ys) "-" = (y - x):ys
          foldingFunc xs num = read num:xs