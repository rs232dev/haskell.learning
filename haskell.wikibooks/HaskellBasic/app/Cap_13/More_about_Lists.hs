module Cap_13.More_about_Lists where


-- ========================================================================== --
-- map function                                                               --
--                                                                            --
-- map is the general solution for applying a function to each and every      --
-- element of a list                                                          --
-- ========================================================================== --

-- from base-4.12.0.0/docs/src/GHC.Base.html#map
-- map :: (a -> b) -> [a] -> [b]
-- map _ []     = []
-- map f (x:xs) = f x : map f xs

map_add1 = map (+1) [1,2,3]
-- Main> map_add1 
-- [2,3,4]

add2:: Num a => a -> a
add2 x = x+2

map_add2 = map add2 [1,2,3]
-- Main> map_add2
-- [3,4,5]


heads:: [[a]] -> [a]
heads = map head
get_heads = heads [[1,2,3],[7,8,9],[5,10,15]]
-- Main> get_heads 
-- [1,7,5]


-- ========================================================================== --
-- Dot Dot Notation                                                           --
-- ========================================================================== --

-- Haskell has a convenient shorthand for writing ordered lists of 
-- regularly-spaced integers.
-- Some examples to illustrate it:

-- -------------------------------------------------------------------------- --
--         Code                 |                     Result                  --
-- -------------------------------------------------------------------------- --
--     [1..10]                  |         [1,2,3,4,5,6,7,8,9,10]              --
--     [2,4..10]                |         [2,4,6,8,10]                        --
--     [5,4..1]                 |         [5,4,3,2,1]                         --
--     [1,3..10]                |         [1,3,5,7,9]                         --
-- -------------------------------------------------------------------------- --

one_to_ten = [1..10]
-- Main> one_to_ten 
-- [1,2,3,4,5,6,7,8,9,10]

-- ========================================================================== --
-- Infinite Lists                                                             --
-- ========================================================================== --

-- Thanks to lazy evaluation, Haskell lists can be infinite. 
-- For example, the following generates the infinite list of integers 
-- starting with 1:   [1..]

-- Infinite lists are useful in practice because Haskell's lazy evaluation never
-- actually evaluates more than it needs at any given moment. In most cases, we 
-- can treat an infinite list like an ordinary one. The program will only go 
-- into an infinite loop when evaluation requires all the values in the list. 

-- So, we can't sort or print an infinite list, but:
-- evens = doubleList [1..]
-- will define "evens" to be the infinite list [2,4,6,8..], and we can
-- then pass "evens" into other functions that only need to evaluate part of the
-- list for their final result. Haskell will know to only use the portion of 
-- the infinite list needed in the end.

-- ========================================================================== --
-- Prelude null fuction                                                       --
-- ========================================================================== --

-- Main> :t null
-- null :: Foldable t => t a -> Bool

-- Main> null []
-- True

-- Main> null [1]
-- False







