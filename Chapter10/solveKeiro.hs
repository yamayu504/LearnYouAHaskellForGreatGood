import Data.List
main = do
    contents <- getContents
    let threes = groupOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c]-> Section a b c) threes
        path = solveKeiro roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The way" ++ pathString
    putStrLn $ "The way" ++ show pathTime

-- 入力値からRoadSystemに
groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)


data Section = Section {getA :: Int,getB :: Int, gettC :: Int}
    deriving(Show)

type RoadSystem = [Section]

heathrowToLoandon :: RoadSystem
heathrowToLoandon = [Section 50 10 10,
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0
                    ]

data Label = A | B | C deriving(Show)
type Path = [(Label,Int)]

-- 最適なpathをSystemから生成
solveKeiro :: RoadSystem -> Path
solveKeiro roadSystem =
    let (bestA,bestB) = foldl roadStep ([],[]) roadSystem
    in if sum (map snd bestA) >=  sum (map snd bestB)
       then reverse bestA
       else reverse bestB 
-- pathを生成する
roadStep :: (Path,Path) -> Section -> (Path,Path)
roadStep (pathA,pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeTOA = timeA + a
        crossTimeTOA   = timeB + b + c
        forwardTimeTOB = timeB + b
        crossTimeTOB   = timeA + a + c
        newPathA = if forwardTimeTOA <= crossTimeTOA
                    then (A,a):pathA
                    else (C,c):(B,b):pathB
        newPathB = if forwardTimeTOB <= crossTimeTOB
                    then (B,b):pathB
                    else (C,c):(A,a):pathA
    in (newPathA,newPathB)