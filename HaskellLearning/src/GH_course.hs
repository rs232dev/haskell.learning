module GH_course where 

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

lc = [1^2 | x <- [1..5]] 
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









