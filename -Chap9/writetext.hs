import System.IO
-- readするだけ
-- main = do
--     handle <- openFile "haiku.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

main = do 
    withFile "haiku.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents