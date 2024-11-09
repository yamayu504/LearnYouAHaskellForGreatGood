import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception


main = do 
    (command:argList) <- getArgs
    dispatch command argList
 
dispatch :: String -> [String] -> IO ()
dispatch "add"  = add
dispatch "view" = view
dispatch "remove" = remove

add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO()
remove [fileName,itemIndex] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TODO Items:"
    mapM_ putStr numberedTasks
    let index = read itemIndex
        newTodoItems = unlines $ delete (todoTasks !! index) todoTasks
    bracketOnError (openTempFile "." "temp") 
        (\(tempName, tempHandle) -> do 
            hClose tempHandle 
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)