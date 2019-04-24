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
-- fa (f b (f c acc))

mylist = 5:10:15:[]   
fsum = foldr (+) 0 mylist
