module Cap_15.Type_Declarations where

-- ========================================================================== --
-- data and constructor functions                                             --
--                                                                            --
-- data is used to define new data types mostly using existing ones as        --
-- building blocks.                                                           --
--                                                                            --
-- Here's a data structure for elements in a simple list of anniversaries.    --
-- ========================================================================== --

-- -------------------------------------------------------------------------- --
-- type names and constructor functions must start with capital letters       --
-- -------------------------------------------------------------------------- --

data Anniversary = Birthday String Int Int Int
                |  Wedding  String String Int Int Int

-- This declares a new data type Anniversary  which can be either a Birthday 
-- or a Wedding.
-- A Birthday contains one string and three integers and a Wedding contains
-- two strings and three integers.

-- Main> :t Birthday
-- Birthday :: String -> Int -> Int -> Int -> Anniversary

-- Main> :t Wedding
-- Wedding :: String -> String -> Int -> Int -> Int -> Anniversary

smith_anniversary = Birthday "john Smith" 1968 7 3

-- Main> :t smith_anniversary 
-- smith_anniversary :: Anniversary

smith_wedding = Wedding "john Smith" "Jane Smith" 1987 3 4
-- Main> :t smith_wedding
-- smith_wedding :: Anniversary

-- ========================================================================== --
-- Deconstructing types                                                       --
--                                                                            --
-- To use our new data types, we must have a way to access their contents.    --
-- ========================================================================== --

showDate :: Int -> Int -> Int -> String
showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
    name ++ " born " ++ showDate year month day

-- Main> showAnniversary smith_anniversary
-- "john Smith born 1968-7-3"

-- -------------------------------------------------------------------------- --
-- Note that the parentheses around the constructor name and the bound        --
-- variables are mandatory; otherwise the compiler or interpreter would not   --
-- take them as a single argument. Also, it is important to have it           --
-- absolutely clear that the expression inside the parentheses is not a call  --
-- to the constructor function, even though it may look just like one.        --
-- -------------------------------------------------------------------------- --



-- ========================================================================== --
-- Type for making type synonyms                                              --
--                                                                            --
-- ========================================================================== --

-- -------------------------------------------------------------------------- --
-- it could be nice to make it clear that the Strings in the Anniversary type --
-- are being used as names while still being able to manipulate them like     --
-- ordinary Strings. This calls for a type declaration:                       --
-- -------------------------------------------------------------------------- --
type Name = String

-- -------------------------------------------------------------------------- --
-- The code above says that a Name is now a synonym for a String.             --
-- Any function that takes a String will now take a Name as well              --
-- (and vice-versa: functions that take Name will accept any String).         --
-- The right hand side of a type declaration can be a more complex type as    --
-- well. For example, String itself is defined in the standard libraries as   --
-- type String = [Char]                                                       --
-- -------------------------------------------------------------------------- --

data Anniversary' = Birthday' Name Int Int Int
                 |  Wedding'  Name String Int Int Int
    
smith_wedding' = Wedding' "john Smith" "Jane Smith" 1987 3 4

-- Main> :t Birthday'
-- Birthday' :: Name -> Int -> Int -> Int -> Anniversary'

