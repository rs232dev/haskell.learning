module Cap_12.Recursion where

-- ========================================================================== --
-- Recursion                                                                  --
-- ========================================================================== --
factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Main> factorial 4
-- 24

-- ========================================================================== --
-- List based Recursion                                                       --
-- ========================================================================== --

-- from Prelude. length  function renamed to length''
length'' :: [a] -> Int
length'' []     = 0
length'' (x:xs) = 1 + length'' xs

-- Main> length'' [9,8,7,6,5]
-- 5

-- from Prelude:
-- (++) []     ys = ys
-- (++) (x:xs) ys = x : xs ++ ys

list_concat = [1,2,3] ++ [7,8,9]
-- Main> list_concat 
-- [1,2,3,7,8,9]

