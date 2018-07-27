module GH_course where 

import Data.Char  -- functions on characters

-- ========================================================================== ==    
-- pag. 9 "a taste of haskell" 
-- *Main> :t fsum
-- fsum :: Num a => [a] -> a
-- ========================================================================== ==    
fsum :: (Num a) =>  [a] -> a
fsum [] = 0
fsum (x:xs) = x + fsum xs

fsum':: (Num a) =>  [a] -> a
fsum' = foldr (+) 0

-- ========================================================================== ==    
-- pag. 10 "a taste of haskell" 
-- *Main> :t fqsort
-- fqsort :: Ord a => [a] -> [a]
--
-- how it works:
--
-- fqsort[3,5,1,4,2]
-- 
-- fqsort [1,2] ++ [3] ++ fqsort [5,4]
--         \        \             \
--          \        \_ [x]        \_ larger
--           \
--            \_ smaller
--
--
-- processing fqsort smaller: fqsort [1,2] ++ ...
--
-- fqsort [] ++ [1] ++ [2] 
--         \     \      \_ larger
--          \     \
--           \     \_ [x]
--            \
--             \_ smaller
--        
--  next step fqsort smaller
--  [] = [] ..so end recursive call then the result is: [1,2]
--   
-- processing fqsort larger: ... ++ fqsort [5,4]
--
-- fqsort [4] ++ [5] ++ [] 
--         \      \      \_ larger
--          \      \
--           \      \_ [x]
--            \
--             \_ smaller
--        
--  next step fqsort larger
--  [] = [] ..so end recursive call then the result is: [4,5]
--
-- so the final result is: [1,2,3,4,5]
-- ========================================================================== ==    
fqsort :: Ord a => [a] -> [a]
fqsort []      = []
fqsort (x:xs)  = fqsort smaller ++ [x] ++ fqsort larger
    where
        smaller = [a | a <- xs,a<=x]
        larger  = [a | a <- xs,a>x]


-- ========================================================================== ==    
-- infinite list .. thanks to lazy evaluation
-- ========================================================================== ==    
infinte_list = [1..]


-- ========================================================================== ==    
--     ________  _____  __   ________
--    /_  __/ / / / _ \/ /  / __/ __/
--     / / / /_/ / ___/ /__/ _/_\ \  
--    /_/  \____/_/  /____/___/___/  
---
-- ========================================================================== ==    
tuples_add :: (Int,Int) -> Int
tuples_add (a,b) = a + b


-- ========================================================================== ==    
--       _______  _____  ___  ___________ 
--      / ___/ / / / _ \/ _ \/  _/ __/ _ \
--     / /__/ /_/ / , _/ , _// // _// // /
--     \___/\____/_/|_/_/|_/___/___/____/ 
--     ______  ___  _____________________  _  ______
--    / __/ / / / |/ / ___/_  __/  _/ __ \/ |/ / __/
--   / _// /_/ /    / /__  / / _/ // /_/ /    /\ \  
--  /_/  \____/_/|_/\___/ /_/ /___/\____/_/|_/___/  
--  
-- ========================================================================== ==    
-- To avoid excess parehtheses when working with curried functions, two simple
-- conventions are adopted:
--
-- The function arrow -> associates to the right:
--
-- Int -> Int -> Int -> Int 
-- Int -> (Int -> (Int -> Int))
--
-- consequently, function application is assumed to associated to the left
-- For example the application:
--
-- mult x y z
-- ((mult x) y) z
-- -------------------------------------------------------------------------- --
add':: Int -> (Int -> Int)
add' x y = x + y

-- ========================================================================== ==    
--     ___  ____  ____  ____  _______  ___  ___  __ ___________
--    / _ \/ __ \/ /\ \/ /  |/  / __ \/ _ \/ _ \/ // /  _/ ___/
--   / ___/ /_/ / /__\  / /|_/ / /_/ / , _/ ___/ _  // // /__  
--  /_/   \____/____//_/_/  /_/\____/_/|_/_/  /_//_/___/\___/  
--   ________  _____  ________
--  /_  __/\ \/ / _ \/ __/ __/
--   / /    \  / ___/ _/_\ \  
--  /_/     /_/_/  /___/___/  
--
-- ========================================================================== ==    
--    
-- Example of polymorphic function:
--
-- Main> :t length
-- length :: Foldable t => t a -> Int
--
-- A types thet contains one ore more type variables is called
-- polimorphic.
-- 
-- length :: [a] -> Int   -- length is a polymhorpic function
--
-- means:  For any type a, the funcyion length has type [a] -> Int.
--
-- Example: Polymorphic function fst:
-- 
-- Main> :t fst
-- fst :: (a, b) -> a
-- -------------------------------------------------------------------------- --       



-- ========================================================================== ==    
--    _______   ___   ________   _________  _  _______________  ___   _____  ________ 
--   / ___/ /  / _ | / __/ __/  / ___/ __ \/ |/ / __/_  __/ _ \/ _ | /  _/ |/ /_  __/ 
--  / /__/ /__/ __ |_\ \_\ \   / /__/ /_/ /    /\ \  / / / , _/ __ |_/ //    / / / 
--  \___/____/_/ |_/___/___/   \___/\____/_/|_/___/ /_/ /_/|_/_/ |_/___/_/|_/ /_/ 
--
-- ========================================================================== ==    
-- class contraint is written in the form  C a, where C is the name of a class
-- and a is a type-variable.
--
-- Example:
-- Main> :t (+)
-- (+) :: Num a => a -> a -> a
--
-- For any type a that is an instance of the class Num of numeric types,
-- the function (+) has type a -> a -> a
--
-- A type that contains one or more class constraints is called overloaded.
--
-- Numbers themselves are overloaded.
-- For example, 3:: Num a => a means that for any numeric type a, the value 3 
-- has type. In this manner, the value 3 could be an integer, a floating-point 
-- number, or more generally a valueof any numeric type, depending on the 
-- context which is used.
-- -------------------------------------------------------------------------- --

-- ========================================================================== ==    
--     ___  ___   _____________  _______   ___   ________________
--    / _ )/ _ | / __/  _/ ___/ / ___/ /  / _ | / __/ __/ __/ __/
--   / _  / __ |_\ \_/ // /__  / /__/ /__/ __ |_\ \_\ \/ _/_\ \  
--  /____/_/ |_/___/___/\___/  \___/____/_/ |_/___/___/___/___/  
-- ========================================================================== ==    
-- Recall: 
--
-- * A type is a collection of related values
--
-- * A class is a collection of types that support certain overloaded 
--   operations called methods.
--
-- Examples:
--
-- Eq
-- Eq is a class that contains type whose values can be compare for equality
-- and inequality using the following two methods:
--
-- Main> :t (==)
-- (==) :: Eq a => a -> a -> Bool 
--
-- Main> :t (/=)
-- (/=) :: Eq a => a -> a -> Bool
--
-- All basic types Bool,Char,String,Int,Integer,Float are instance of
-- the Eq class.    
--
-- Other basic classes are: Ord, Show, Read, etc..
--
-- Readable types:
-- read :: Read a => String -> a
-- 
-- The class Read contains type whose values can be converted from
-- string of caharacters.
-- 
boolean_false = read "False"::Bool 

-- Main> :t boolean_false
-- boolean_false :: Bool

int_from_string = read "12"::Int 

-- Main> :t int_from_string
-- int_from_string :: Int
--
--
-- Numeric types:
-- This class contains types whose value are numeric and as such processed
-- using the following six methods:
--
-- (+)    :: Num a => a -> a -> a
-- (-)    :: Num a => a -> a -> a
-- (*)    :: Num a => a -> a -> a
-- negate :: Num a => a -> a
-- abs    :: Num a => a -> a
-- signum :: Num a => a -> a
--
-- Other basic classes are: Integral (integral types), Fractional (fractional types)
-- -------------------------------------------------------------------------- --
--
-- swap :: (b, a) -> (a, b)
-- swap (x,y) = (y,x)
--
-- swap :: (b, a) -> (a, b)
-- pair x y = (x,y)
-- 
-- twice :: (t -> t) -> t -> t
-- twice f x = f(f(x))

-- ========================================================================== ==    
--      ___  _____________  _______  _______
--     / _ \/ __/ __/  _/ |/ /  _/ |/ / ___/
--    / // / _// _/_/ //    // //    / (_ / 
--   /____/___/_/ /___/_/|_/___/_/|_/\___/  
--    ______  ___  _____________________  _  ______
--   / __/ / / / |/ / ___/_  __/  _/ __ \/ |/ / __/
--  / _// /_/ /    / /__  / / _/ // /_/ /    /\ \  
-- /_/  \____/_/|_/\___/ /_/ /___/\____/_/|_/___/  
--
-- ========================================================================== ==    
-- Conditional Expression:
abs :: Int -> Int
abs n = if n >=0 then n else -n
-- putStrLn ("abs :" ++  show ( GH_course.abs (-4)));

-- -------------------------------------------------------------------------- --
--     _______  _____   ___  ___  _______ 
--    / ___/ / / / _ | / _ \/ _ \/ __/ _ \
--   / (_ / /_/ / __ |/ , _/ // / _// // / (GUARDED)
--   \___/\____/_/ |_/_/|_/____/___/____/ 
--    ________  __  _____ ______________  _  ______
--   / __/ __ \/ / / / _ /_  __/  _/ __ \/ |/ / __/
--  / _// /_/ / /_/ / __ |/ / _/ // /_/ /    /\ \  
-- /___/\___\_\____/_/ |_/_/ /___/\____/_/|_/___/  
--
-- As an alternative to conditional expressions, function ca also be defined
-- using guarded equations.
-- The symbol | is read as such that
-- -------------------------------------------------------------------------- --
abs' :: Int -> Int
abs' n | n >=0 = n
       | otherwise = -n

-- -------------------------------------------------------------------------- --
--    ___  ___ ___________________  _  __             __      __   _          
--   / _ \/ _ /_  __/_  __/ __/ _ \/ |/ / __ _  ___ _/ /_____/ /  (_)__  ___ _ 
--  / ___/ __ |/ /   / / / _// , _/    / /  ' \/ _ `/ __/ __/ _ \/ / _ \/ _ `/
-- /_/  /_/ |_/_/   /_/ /___/_/|_/_/|_/ /_/_/_/\_,_/\__/\__/_//_/_/_//_/\_, /
--                                                                     /___/
-- -------------------------------------------------------------------------- --
not :: Bool -> Bool
not False = True
not True  = False

-- wildcard pattern _ match any value
(&&) :: Bool -> Bool -> Bool
True && b  = b
False && _ = False

-- -------------------------------------------------------------------------- --
--   ________  _____  __   ____  ___  ___ ___________________  _  ______
--  /_  __/ / / / _ \/ /  / __/ / _ \/ _ /_  __/_  __/ __/ _ \/ |/ / __/
--   / / / /_/ / ___/ /__/ _/  / ___/ __ |/ /   / / / _// , _/    /\ \  
--  /_/  \____/_/  /____/___/ /_/  /_/ |_/_/   /_/ /___/_/|_/_/|_/___/  
-- -------------------------------------------------------------------------- --
fst :: (a,b) -> a
fst (x,_)  = x

snd :: (a,b) -> b
snd (_,y)  = y

-- -------------------------------------------------------------------------- --
--    __   ______________  ___  ___ ___________________  _  ______
--   / /  /  _/ __/_  __/ / _ \/ _ /_  __/_  __/ __/ _ \/ |/ / __/
--  / /___/ /_\ \  / /   / ___/ __ |/ /   / / / _// , _/    /\ \  
-- /____/___/___/ /_/   /_/  /_/ |_/_/   /_/ /___/_/|_/_/|_/___/  
-- -------------------------------------------------------------------------- --
test :: [Char] -> Bool
test ['a',_,_] = True
test _         = False

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

-- ========================================================================== ==    
--      __   ___   __  ______  ___  ___ 
--     / /  / _ | /  |/  / _ )/ _ \/ _ |
--    / /__/ __ |/ /|_/ / _  / // / __ |
--   /____/_/ |_/_/  /_/____/____/_/ |_|
--    _____  _____  ___  ____________________  _  ______
--   / __/ |/_/ _ \/ _ \/ __/ __/ __/  _/ __ \/ |/ / __/
--  / _/_>  </ ___/ , _/ _/_\ \_\ \_/ // /_/ /    /\ \  
-- /___/_/|_/_/  /_/|_/___/___/___/___/\____/_/|_/___/  
--
-- As an alternative to defining functions using equations, functions can
-- also be constructed using lambda expressions, which comprise a pattern
-- for each of the arguments, a body that specifies how the result can be
-- calculated in terns of the arguments, but do not give a name for the
-- function itself. In other words lamda expressions are nameless functions.
-- ========================================================================== ==    
--    
-- Example
-- Main> (\x -> x + x) 2
-- 4

add'' :: Int -> (Int -> Int)
add'' = \x -> (\y -> x + y)

odd :: Int -> [Int]   
odd n = map (\x -> x*2 +1) [0..n-1]

safetail :: [a] -> [a]

-- conditional expression
safetail xs = if null xs then [] else tail xs

-- guarded equation
{--
safetail xs 
    | null xs   = []
    | otherwise = tail xs

-- pattern matching
-- safetail (_:xs) = xs
-- safetail []     = []
--}

-- ========================================================================== ==    
--    
--      __   ______________
--     / /  /  _/ __/_  __/
--    / /___/ /_\ \  / /   
--   /____/___/___/ /_/    
--   _________  __  ______  ___  ______ _______  ______________  _  __
--  / ___/ __ \/  |/  / _ \/ _ \/ __/ // / __/ |/ / __/  _/ __ \/ |/ /
-- / /__/ /_/ / /|_/ / ___/ , _/ _// _  / _//    /\ \_/ // /_/ /    / 
-- \___/\____/_/  /_/_/  /_/|_/___/_//_/___/_/|_/___/___/\____/_/|_/  
--
-- In mathematics, the comprehension notation can be used to construct new sets
-- from existing sets.
-- In Haskell, a similar comprehension notation can be used to construct new
-- lists from existing lists.
-- ========================================================================== ==                                                                     

-- | is read as such that
-- <- is read as drawn from
-- and the expression <- [1..5] is called generator

lc = [x^2 | x <- [1..5]] 
-- [1,4,9,16,25]

-- -------------------------------------------------------------------------- --
-- A list comprehension can have more than one generator being separeted by 
-- commas:
-- -------------------------------------------------------------------------- --
lcm =  [ (x,y) | x <- [1,2,3] , y <- [4,5] ]

-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- -------------------------------------------------------------------------- --
-- Later generators can also depend upon the values of variables from earlier 
-- generators:
-- -------------------------------------------------------------------------- --
lcdepends= [ (x,y) | x <- [1..3] , y <- [x .. 3] ]

-- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-- -------------------------------------------------------------------------- --
-- example of conac function by list comprehension
-- -------------------------------------------------------------------------- --
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- example
--concat [[1,3,4],[],[10,11],[12]]
--[1,3,4,10,11,12]

-- -------------------------------------------------------------------------- --
-- list comprehension with wildcard pattern
-- -------------------------------------------------------------------------- --
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

-- example
-- firsts [(1,3),(4,7),(5,9)]
-- [1,4,5]

-- -------------------------------------------------------------------------- --
--    __   _____                        __  
--   / /  / ___/ ___ ___ _____ ________/ /__
--  / /__/ /__  / _ `/ // / _ `/ __/ _  (_-<
-- /____/\___/  \_, /\_,_/\_,_/_/  \_,_/___/
--              /___/ 
--
-- List comprehension can also use logical expressions called guards to filter
-- the values produced by earlier generators.
-- -------------------------------------------------------------------------- --
lcg :: [Int] -> [Int]
lcg xs = [x | x <- xs , even x]
--                          \__ list comprehension guard

-- example:
-- lcg [1,2,3,4,5,6,7,8,9,10,11,12]
-- [2,4,6,8,10,12]


find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k'] 
--                              \__ list comprehension guard
-- example:
-- find 'b' [('a',1),('b',2),('c',3),('b',4),('d',5),('z',9)]
-- [2,4]

-- -------------------------------------------------------------------------- --
--     ______      _          
--    / __/ /_____(_)__  ___ _
--   _\ \/ __/ __/ / _ \/ _ `/
--  /___/\__/_/ /_/_//_/\_, / 
--                     /___/  
--   _____                        __                _         
--  / ___/__  __ _  ___  _______ / /  ___ ___  ___ (_)__  ___ 
-- / /__/ _ \/  ' \/ _ \/ __/ -_) _ \/ -_) _ \(_-</ / _ \/ _ \
-- \___/\___/_/_/_/ .__/_/  \__/_//_/\__/_//_/___/_/\___/_//_/
--               /_/                                          
-- -------------------------------------------------------------------------- --
-- String is just an abbreviation for the list of characters
-- So, for the same reason, list comprehension can also be used to define
-- functions on strings.

-- lowers function return the number of the lowercase letters
lowers:: String -> Int
lowers xs = length [ x | x <- xs , x >= 'a' Prelude.&&  x <= 'z']

-- example
-- lowers ['H','a','S','k','E','l','l']
-- 4

-- ========================================================================== ==    
--    ___                       _            ____              __  _             
--   / _ \___ ______ _________ (_)  _____   / __/_ _____  ____/ /_(_)__  ___  ___
--  / , _/ -_) __/ // / __(_-</ / |/ / -_) / _// // / _ \/ __/ __/ / _ \/ _ \(_-<
-- /_/|_|\__/\__/\_,_/_/ /___/_/|___/\__/ /_/  \_,_/_//_/\__/\__/_/\___/_//_/___/
--
-- Recursive functions : Function defined in terms of themselves
-- ========================================================================== ==    
fac :: Int -> Int
fac 0 = 1              -- base case
fac n = n * fac(n-1)   -- recursive case

-- example:
-- fac 4
-- 24

-- -------------------------------------------------------------------------- --
-- Recursion on lists
-- -------------------------------------------------------------------------- --
lproduct:: Num a => [a] -> a
lproduct []     = 1
lproduct (x:xs) = x * (lproduct xs) 

-- example:
-- lproduct [2,3,4]
-- 24

llength:: [a] -> Int
llength [] = 0
llength (_:xs) = 1 + llength xs

-- example:
-- llength [1,2,3,4,5]
-- 5

(+++)::[a] -> [a] ->[a]
[] +++ ys  = ys
(x:xs) +++ ys = x : xs +++ ys

--  [1,2,3] +++ [4,5]
--  1 : ([2,3] +++ [4,5])
--  1 : 2 : ([3] +++ [4,5])
--  1 : 2 : 3 : ([] +++ [4,5])
--  1 : 2 : 3 : [4,5])
-- 
-- result:
-- [1,2,3,4,5]

-- example:
-- [1,2,3,4] +++ [7,8,9]
-- [1,2,3,4,7,8,9]


insert:: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) 
            | x<=y      = x:y:ys
            | otherwise = y:insert x ys

-- example
-- insert 3 [1,2,4,5]
-- [1,2,3,4,5]

-- ========================================================================== ==    
--    __ ______    ____              __  _             
--   / // / __ \  / __/_ _____  ____/ /_(_)__  ___  ___
--  / _  / /_/ / / _// // / _ \/ __/ __/ / _ \/ _ \(_-<
-- /_//_/\____/ /_/  \_,_/_//_/\__/\__/_/\___/_//_/___/
-- 
-- ========================================================================== ==      

add_int :: Int -> Int -> Int
add_int x y = x + y

-- means

add_int' :: Int -> (Int -> Int)
add_int' = \x -> (\y -> x +y)

-- and states that add_int is a function that takes an integer x and returns
-- a function, which in turn take another integer y and returns their sum x + y.


-- -------------------------------------------------------------------------- --
-- In Haskell it also pemissible to define functions that take functions as   --
-- arguments.                                                                 --
-- -------------------------------------------------------------------------- --
twice :: (a -> a) -> a -> a
twice f x = f ( f x )

-- example
-- twice (*2) 2
-- 8

-- twice reverse [1,2,3]
-- [1,2,3]


-- -------------------------------------------------------------------------- --
-- HOF processing lists                                                       --
-- -------------------------------------------------------------------------- --

-- > :t map
-- map :: (a -> b) -> [a] -> [b]

-- example
-- map (*2) [1,2,3]
-- [2,4,6]

-- mymap
mymap :: (a -> b) -> [a] -> [b]
mymap f xs = [ f x | x <- xs]

-- example
-- mymap (+1) [10,11,12]
-- [11,12,13]

-- map is a polymorphic function that can be applied to lists of any type.
-- map can be applied to itself to process nested lists
-- 
-- mymap (mymap (+1)) [ [1,2,3] , [4,7] ]
-- [[2,3,4],[5,8]]

-- step .1 applying the outer map
-- [ mymap (+1) [1,2,3], mymap (+1) [4,7] ]

-- applying the inner map
-- [[2,3,4],[5,8]]

-- -------------------------------------------------------------------------- --
-- finally the function map can also be defined using recursion               --
-- -------------------------------------------------------------------------- --
mymap' :: (a->b) -> [a] -> [b]
mymap' f []     = []
mymap' f (x:xs) =  f x : mymap' f xs

-- -------------------------------------------------------------------------- --
-- hof 'filter' by list comprehension                                         --
-- -------------------------------------------------------------------------- --
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

-- example
-- GH_course.filter even [1,12,4,21]
-- [12,4]

-- -------------------------------------------------------------------------- --
-- hof all/any                                                                --
-- -------------------------------------------------------------------------- --

-- > :t any
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
--any even [1,12,4,21]
--True

-- > :t all
-- all :: Foldable t => (a -> Bool) -> t a -> Bool
-- all even [1,12,4,21]
-- False

-- -------------------------------------------------------------------------- --
-- hof 'foldr'                                                                --
--                                                                            --
-- The HOF foldr (abbreviation fold right) encapsulates the following pattern --
-- of recursion for defining functions on lists. See next examples:           -- 
-- -------------------------------------------------------------------------- --
--
-- > :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--                             |           |      \    \_result  
--                       binary function   start   \
--                                        element   \_list
-- sum []     = 0
-- sum (x:xs) = x + sum xs
--
-- product []     = 1
-- product (x:xs) = x * product xs
--
-- or []     = False
-- or (x:xs) = x || or xs
--
-- and []     = True
-- and (x:xs) = x && and xs

-- now rewrite the above function by HOF foldr 

sum :: Num a => [a] -> a
sum = foldr (+) 0

-- example
-- GH_course.sum [0,1,2,3,4,5,6,7,8,9]
-- 45

product :: Num a => [a] -> a
product = foldr (*) 1

-- example
-- GH_course.product [3,7,2]
-- 42

or :: [Bool] -> Bool
or = foldr (||) False

-- example
-- GH_course.or [True,False,False]
-- True

and :: [Bool] -> Bool
and = foldr (Prelude.&&) True

-- example
-- GH_course.and [True,False,False]
-- False


-- -------------------------------------------------------------------------- --
-- hof 'foldl'                                                                --
--                                                                            --
-- Function on list using an operator that is assumed to associates to the    --
-- left.                                                                      -- 
-- -------------------------------------------------------------------------- --
--
-- > :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--                             |           |      \    \_result  
--                       binary function   start   \
--                                        element   \_list


-- ========================================================================== ==      
--             ____                    __                                     --
--            / __ \___  ___ _______ _/ /____  ____                           --
--     _     / /_/ / _ \/ -_) __/ _ `/ __/ _ \/ __/                           --
--    (_)    \____/ .__/\__/_/  \_,_/\__/\___/_/                              --
-- composition   /_/                                                          --
--                                                                            --
-- The HOF . operator returns the composition of two functions as a single    --
-- function.                                                                  -- 
-- ========================================================================== ==
--                                                                            --
-- > :t (.)                                                                   --
-- (.) :: (b -> c) -> (a -> b) -> a -> c                                      --
--                                                                            --
-- f . g = \x -> f (g x)                                                      --
--                                                                            --
-- example                                                                    --
-- comp = Prelude.sum . map (+1)                                              --
-- comp [1,2,3]                                                               --
-- 9                                                                          --
--                                                                            --
-- The composition operator is associative:                                   --
--                                                                            -- 
-- f . (g . h) = (f .g) . h   for any function f,g,h                          --
--                                                                            --
-- Composition also has an identity, given by the identity function:          --
--                                                                            --
-- id :: a -> a                                                               --
-- id = \x -> x                                                               --
--                                                                            --
-- The identity function simply returns its argument unchanged, and has       --
-- the property that:                                                         --
--                                                                            --
--  id . f = f                                                                --
--                                                                            --
--  and                                                                       --
--                                                                            --
--  f . id = f                                                                --
--                                                                            --
-- for any function f.                                                        --
--                                                                            --
-- The identity function provides a suitable starting point  for a sequence   --
-- of compositions.                                                           --
-- -------------------------------------------------------------------------- --


-- Exercise: Binary String Transmitter
-- synonym declaration for the type integers:
type Bit = Int

-- a binary number, represented as a list of bits, can converted in to an 
-- integer by simply evaluating the appropriate weighted sum:
bin2int :: [Bit] -> Int
bin2int bits = Prelude.sum [ w * b | (w,b) <- zip weight bits]
                       where weight = iterate (*2) 1

-- The HOF iterate produces an infinite list by applying a function an
-- increasing number of times to a value
-- iterate f x == [x, f x, f (f x), ...]
-- so the generator <- zip weight bits makes:
--
-- [(1,1),(2,0),(4,1)]
--
--               lsb   msb
-- then > bin2int [1,1,0]
-- 3

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- example
-- > int2bin 8
-- [0,0,0,1]

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- take n, applied to a list xs, returns the prefix of xs of length n, 
-- or xs itself if n > length xs:
-- take 5 "Hello World!" == "Hello"
--
-- repeat x is an infinite list, with x the value of every element.
-- The library function repeat:: a -> [a] produce an infinite list of 
-- copy of a value, but lazy evaluation ensure that only as many elements
-- as required by the context will actually be produced.    
--
-- example:
-- > make8 [1,0,1]
-- [1,0,1,0,0,0,0,0]

-- We can now define a function that encodes a string of characters as
-- a list of bits converting each character into a Unicode number,
-- converting each such number into an eight-bit binary number.
-- > :t ord
-- ord :: Char -> Int
encode :: String -> [Bit]
encode = Prelude.concat . map (make8 . int2bin .ord)

-- example
-- encode "abc"
-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

-- decode a list of bits

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
--
-- example:
-- > chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
--   [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8
--
-- example 
-- > decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-- "abc"


-- ========================================================================== ==
--                                                                            -- 
--     ______                             __  _______                         --
--    /_  __/_ _____  ___   ___ ____  ___/ / / ___/ /__ ____ ___ ___ ___      --
--     / / / // / _ \/ -_) / _ `/ _ \/ _  / / /__/ / _ `(_-<(_-</ -_|_-<      --
--    /_/  \_, / .__/\__/  \_,_/_//_/\_,_/  \___/_/\_,_/___/___/\__/___/      --
--        /___/_/                                                             --
--                                                                            --
-- ========================================================================== ==

-- -------------------------------------------------------------------------- --
--     ______                ___          __              __  _               --
--    /_  __/_ _____  ___   / _ \___ ____/ /__ ________ _/ /_(_)__  ___       --
--     / / / // / _ \/ -_) / // / -_) __/ / _ `/ __/ _ `/ __/ / _ \/ _ \      --
--    /_/  \_, / .__/\__/ /____/\__/\__/_/\_,_/_/  \_,_/\__/_/\___/_//_/      --
--        /___/_/                                                             --
--                                                                            --
-- -------------------------------------------------------------------------- --
-- create type synonym:                                                       --
-- examples:                                                                  --
type SString = [Char]
type Pos     = (Int,Int)
type Trans   = Pos -> Pos

-- NB. Type declaration cannot be recursive.
--  type Tree = (Int, [Tree]) error!

-- type declarations can also be parametrised by other types:
type Pair a = (a,a)

-- type declarations with more than one parameter are possible too:
type Assoc k v = [(k,v)]

-- -------------------------------------------------------------------------- --
--    ___       __         ___          __              __  _                 --
--   / _ \___ _/ /____ _  / _ \___ ____/ /__ ________ _/ /_(_)__  ___         --
--  / // / _ `/ __/ _ `/ / // / -_) __/ / _ `/ __/ _ `/ __/ / _ \/ _ \        --
-- /____/\_,_/\__/\_,_/ /____/\__/\__/_/\_,_/_/  \_,_/\__/_/\___/_//_/        --
--                                                                            --
-- A complete new type, as opposed to a synonym for an existing type, can be  --
-- declared by specifying its value using 'data' declaration                  --
-- -------------------------------------------------------------------------- --

--  type                   data
-- constructor         constructors
--    \                    \
data MyBool     =   MyFalse | MyTrue

--  type                   data
-- constructor         constructors
--    \                    \
data MyType     =   Type1 | Type2

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)  = (x+1,y)
move West (x,y)  = (x-1,y)

-- The constructor in data declaration can also have arguments:
data Shape = Circle Float |  Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r)  = pi * r^2
area (Rect x y)  = x * y

-- The Data constructor Rec and Circle are actually constructor functions, 
-- which produce results of type Shape.
--
-- >:t Rect
-- Rect :: Float -> Float -> Shape
--
-- :t Circle
-- Circle :: Float -> Shape

-- Data declaration themselves can also be parametrised:

--data Maybe a = Nothing | Just a 
-- the above deta declaration is commented to avoid ambigous occurrence

safediv :: Int -> Int -> Maybe Int
safediv  _ 0 = Nothing
safediv  m n = Just (m `div` n)

-- example
-- > safediv 2 0
-- Nothing

-- > safediv 4 2
-- Just 2

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- example
-- > safehead []
-- Nothing

-- > safehead [1,2,3]
-- Just 1


-- -------------------------------------------------------------------------- --
--    _  __           __                                                      --
--   / |/ /__ _    __/ /___ _____  ___                                        -- 
--  /    / -_) |/|/ / __/ // / _ \/ -_)                                       --
-- /_/|_/\__/|__,__/\__/\_, / .__/\__/                                        --
--                     /___/_/                                                --
--    ___          __              __  _                                      --
--   / _ \___ ____/ /__ ________ _/ /_(_)__  ___                              --
--  / // / -_) __/ / _ `/ __/ _ `/ __/ / _ \/ _ \                             --
-- /____/\__/\__/_/\_,_/_/  \_,_/\__/_/\___/_//_/                             --
--                                                                            -- 
-- If a new type has a single constructor with a single arument, then it can  --
-- also be declared using 'newtype' nechanism.                                --
--                                                                            -- 
-- -------------------------------------------------------------------------- --
newtype Nat = N Int

-- -------------------------------------------------------------------------- --
--   _______                            __                                    --
--  / ___/ /__ ____ ___   ___ ____  ___/ /                                    --
-- / /__/ / _ `(_-<(_-<  / _ `/ _ \/ _  /                                     --
-- \___/_/\_,_/___/___/  \_,_/_//_/\_,_/                                      --
--      _          __                                                         --
--     (_)__  ___ / /____ ____  _______                                       --
--    / / _ \(_-</ __/ _ `/ _ \/ __/ -_)                                      --
--   /_/_//_/___/\__/\_,_/_//_/\__/\__/                                       --
--      __        __              __  _                                       --
--  ___/ /__ ____/ /__ ________ _/ /_(_)__  ___  ___                          --
-- / _  / -_) __/ / _ `/ __/ _ `/ __/ / _ \/ _ \(_-<                          --
-- \_,_/\__/\__/_/\_,_/_/  \_,_/\__/_/\___/_//_/___/                          --
--                                                                            --
-- -------------------------------------------------------------------------- -- 
-- commented because is declared in the standard Prelude:
--
-- class Eq a where
--    (==), (/==) :: a -> a -> Bool
--
-- x/= y = not (x == y) -- default declaration
-- 
-- This declarations states that for a type 'a' to be an instance of the class
-- Eq, it must support equality and inequality operator of the specified type.
--
-- In fact, because a default definition has already been included for the /=
-- operator, declaring an instance require  only requires a definition for the
-- == operator .     
-- example:
--
-- instance Eq Bool where
--    False == False = False
--    True  == True  = True
--    _     == _     = False
--
-- NB: Only type that are declared using data and newtype mechanisms can be
-- made into instances of classses.
--
-- Classes can also be extended to form new classes:
-- -------------------------------------------------------------------------- -- 


