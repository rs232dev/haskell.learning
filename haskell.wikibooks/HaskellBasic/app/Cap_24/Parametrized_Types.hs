module Cap_24.Parametrized_Types where

-- ========================================================================== --
-- Parametrized_Types                                                         --
-- ========================================================================== --

-- Parameterized types are similar to "generic" or "template" types in other 
-- languages. 

-- A parameterized type takes one or more type parameters. 
-- For example, the Standard Prelude type Maybe is defined as follows:

-- data Maybe a = Nothing | Just a

--This says that the type Maybe takes a type parameter a. You can use this to 
-- declare, for example:

lookupBirthday :: [Anniversary] -> String -> Maybe Anniversary

-- The lookupBirthday function takes a list of birthday records and a string 
-- and returns a Maybe Anniversary. 
-- The usual interpretation of such a type is that if the name given through
-- the string is found in the list of anniversaries the result will be Just the
-- corresponding record; otherwise, it will be Nothing. 

--Maybe is the simplest and most common way of indicating failure in Haskell.

-- It is also sometimes seen in the types of function arguments, as a way to
-- make them optional (the intent being that passing Nothing amounts to 
-- omitting the argument).
-- You can parameterize type and newtype declarations in exactly the same way.
-- Furthermore you can combine parameterized types in arbitrary ways to 
-- construct new types.

-- -------------------------------------------------------------------------- --
-- More than one type parameter                                               --
-- -------------------------------------------------------------------------- --
data Either a b = Left a | Right b

pairOff :: Int -> Either String Int
    pairOff people
    | people < 0    = Left "Can't pair off negative number of people."
    | people > 30   = Left "Too many people for this activity."
    | even people   = Right (people `div` 2)
    | otherwise     = Left "Can't pair off an odd number of people."


    
    
    
    