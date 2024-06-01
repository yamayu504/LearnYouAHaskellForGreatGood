import Distribution.Simple.Utils (xargs)
import Text.XHtml (height)
{-# OPTIONS -Wall -Werror #-}
doubleMe x = x + x
doubleUs x y= x * 2 + y * 2
doubleSmallNumber x = if x > 100
                            then x
                            else x*2
boomBang xs = [if x<10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lucky :: Int -> String
lucky 7 = "Lucky Numver Seven"
lucky x = "Sorry, you are not lucky!"

factorial :: Int -> Int
factorial 0=1
factorial n =n * factorial(n-1)

head' :: [a] -> a
head' [] = error "error"
head' (x:_) = x

firstLetter :: String -> String
firstLetter "" = "Enpty"
firstLetter all@(x:xs) = "the first letter of "++ all ++"is" ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
 | weight /height ^2 <= 18.5 = "aa"
 | weight /height ^2 <= 25.0 = "bb"
 | weight /height ^2 <= 30.0 = "cc"
 | otherwise = "dd"

cylinder :: Double -> Double ->Double
cylinder r h = 
    let sideArea =2*pi*r*h
        topArea = pi * r^2
    in sideArea +2 *topArea
    