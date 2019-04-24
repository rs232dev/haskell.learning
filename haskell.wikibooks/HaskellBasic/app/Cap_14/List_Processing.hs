module Cap_14.List_Processing where

-- ========================================================================== --
-- Folds                                                                      --
-- ========================================================================== --

-- Like map, a fold is a higher order function that takes a function and a list.
-- However, instead of applying the function element by element, the fold uses
-- it to combine the list elements into a result value.

--Let's look at a few concrete examples.
-- sum could be implemented as:

-- Example: sum

-- sum :: [Integer] -> Integer
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- Example: product
-- product :: [Integer] -> Integer
-- product []     = 1
-- product (x:xs) = x * product xs

-- All these examples show a pattern of recursion known as a fold. Think of the
-- name referring to a list getting ”folded up” into a single value or to a 
-- function being "folded between" the elements of the list.
-- 
-- Prelude defines four fold functions: foldr, foldl, foldr1 and foldl1

-- ========================================================================== --
-- foldr                                                                      --
--                                                                            --
-- The right-associative foldr folds up a list from the right to left.        --
-- ========================================================================== --

-- As it proceeds, foldr uses the given function to combine each of the elements
-- with the running value called the accumulator. 
-- When calling foldr, the initial value of the accumulator is set as an 
-- argument.

--  foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- ex: 
-- mylist  = a:b:c:[]

-- foldr f acc mylist
-- f a (f b (f c acc))

mylist' = 5:10:15:[]   
fsum = foldr (+) 0 mylist'

fsum'' = foldr (-) 0 mylist'

-- 15 - 0 = 15
--         _/
--        /
-- 10 - 15  = -5
--        ____/     
--       /
-- 5 - (-5)  = 10

-- Main> fsum''
-- 10


-- ========================================================================== --
-- foldl                                                                      --
--                                                                            --
-- The left-associative foldl processes the list in the opposite direction,   --
-- starting at the left side with the first element.                          --
-- ========================================================================== --


-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f acc []     =  acc
-- foldl f acc (x:xs) =  foldl f (f acc x) xs

-- ex: 
-- mylist  = a:b:c:[]

-- foldl f acc mylist
-- f ( f ( f acc a) b) c

fsum' = foldl (-) 0 mylist'

-- 0 - 5 = -5
--   ______ /
--  /
-- -5 - 10  = -15
--    _________/
--   /
-- -15 -15  = - 30

-- Main> fsum'
-- -30

-- folds and laziness
--
-- One reason that right-associative folds are more natural in Haskell than 
-- left-associative ones is that right folds can operate on infinite lists. 
-- A fold that returns an infinite list is perfectly usable in a larger 
-- context that doesn't need to access the entire infinite result. 
-- In that case, foldr can move along as much as needed and the compiler will
-- know when to stop. However, a left fold necessarily calls itself recursively
-- until it reaches the end of the input list (because the recursive call is 
-- not made in an argument to f). Needless to say, no end will be reached if an
-- input list to foldl is infinite.


-- ========================================================================== --
-- Scans                                                                      --
-- ========================================================================== --

-- "scan" is like a cross between a map and a fold. 
-- Folding a list accumulates a single return value, whereas mapping puts each
-- item through a function returning a separate result for each item. 
-- A scan does both: it accumulates a value like a fold, but instead of 
-- returning only a final value it returns a list of all the intermediate 
-- values.

-- Main> :t scanr
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]

fsum''' = scanr (+) 0 mylist'
-- Main> fsum'''
-- [30,25,15,0]

fsum'''' = scanl (+) 0 mylist'
-- Main> fsum''''
-- [0,5,15,30]

-- ========================================================================== --
-- filter                                                                     --
--                                                                            --
-- A common operation performed on lists is filtering -  generating a new     --
-- list composed only of elements of the first list that meet a certain       --
-- condition.                                                                 --
-- A simple example: making a list of only even numbers from a list of        --
-- integers.                                                                  --
-- ========================================================================== --

-- Main> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

new_list = [20,15,1,7,30,3,40,5,9]
flist =  filter (\x -> x > 8) new_list

-- Main> flist 
-- [20,15,30,40,9]


-- ========================================================================== --
-- List comprehensions                                                        --
--                                                                            --
-- List comprehensions are syntactic sugar for some common list operations,   --
-- such as filtering.                                                         --
-- For instance, instead of using the Prelude filter, we could write          --
-- retainEven like this:                                                      --
--                                                                            --
-- ========================================================================== --

isEven n = n `mod` 2 == 0
retainEven es = [n | n <- es, isEven n]
even_list = retainEven new_list

-- STEP 1:
-- (Starting from the middle) Take the list es and draw (the "<-") each of its
-- elements as a value n.

-- STEP 2:
-- (After the comma) For each drawn n test the boolean condition isEven n.

-- STEP 3:
-- (Before the vertical bar) If (and only if) the boolean condition is 
-- satisfied, append n to the new list being created (note the square brackets
-- around the whole expression)


