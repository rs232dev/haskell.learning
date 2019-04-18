module WriterCategory where 

import Data.Char

import Data.Char (isSpace)



type Writer a = (a,String) {-- The type Writer is parameterized by a type variable a and is equivalent to a pair of a and String. --}

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)

f >=> g = \x -> 
    let (y, s1) = f x
        (z, s2) = g y
    in (z, s1 ++ s2)

return :: a -> Writer a
return x = (x, "")    

upCase :: String -> Writer String
upCase s = (map toUpper s, "upCase ")

count :: String -> Writer Int
count s = (length s, "count ")

trim :: String -> Writer String
trim s = (dropWhile isSpace  s, "trim ")

x = (trim >=> upCase) >=> count
y = trim >=> (upCase >=> count)

k = (trim >=> WriterCategory.return)
z = (WriterCategory.return >=> trim)
