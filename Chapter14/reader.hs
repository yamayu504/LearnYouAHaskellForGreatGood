type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)
h a xs = ((),a:xs)


