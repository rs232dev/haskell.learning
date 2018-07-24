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
