import Control.Monad
import Data.Char

main = do 
    contents <- getContents
    putStr (shortLineOnly contents)

shortLineOnly :: String -> String
-- lines は、文字列全体を改行で分割して [String] 型（各行がリストの要素になったもの）に変換します。
-- filter (\line -> length line < 10) は、各行の長さが10文字未満のものだけをフィルタリングして返します。
-- unlines は、リスト [String] の各要素を改行でつないで1つの文字列に戻します。
shortLineOnly = unlines . filter (\line -> length line <20) . lines