module Cap_18.Operators_and_Sections where
    
-- ========================================================================== --
-- Operators and Sections                                                     --
-- ========================================================================== --

-- In Haskell, any function that takes two arguments and has a name consisting
-- entirely of  non-alphanumeric characters is considered an operator. 
-- The most common examples are the  arithmetical ones like addition (+) and 
-- subtraction (-). 
-- Unlike other functions, operators are normally used infix (written between
-- the two arguments). 
-- All operators can also be surrounded with parentheses and then used prefix
-- like other functions:

-- these are the same:

-- 2 + 4
-- (+) 2 4

-- If you have a "normal" prefix function and want to use it as an operator,
-- simply surround it with backticks:

fun_d:: Int -> Int -> Int
fun_d x y = x + y

-- Main> fun_d 1 3
-- 4

-- Main> 1 `fun_d` 3
-- 4



