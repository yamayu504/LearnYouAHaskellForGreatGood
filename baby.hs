import Distribution.Simple.Utils (xargs)
import Text.XHtml (height, abbr, name)
import Distribution.Compat.Lens (_1)
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

maxium' :: (Ord a) => [a] -> a
maxium' [] = error "maximum of empty list!"
maxium' [x] = x
maxium' (x:xs) = max x (maxium' xs)

replicate' :: Int -> a-> [a]
replicate' n x
 | n <= 0    = []
 | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []

zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quickSort :: (Ord a) => [a] ->[a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = [a | a <-xs, a<=x]
        larger = [a | a<-xs, a>x]
    in quickSort smallerOrEqual ++ [x] ++ quickSort larger

multiTree :: Int -> Int -> Int -> Int
multiTree x y z = x*y*z

applyTwice :: (a->a) -> a -> a
applyTwice f x =  f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f = g
    where g x y = f y x

chain :: Int -> [Int]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | otherwise = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length ( filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
    
