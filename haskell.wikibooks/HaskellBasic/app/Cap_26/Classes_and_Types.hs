module Cap_26.Classes_and_Types where

-- ========================================================================== --
-- Classes and types                                                          --
-- ========================================================================== --    

-- Broadly speaking, the point of type classes is to ensure that certain 
-- operations will be available for values of chosen types. 
-- For example, if we know a type belongs to (or, to use the jargon, 
-- instantiates) the class Fractional, then we are guaranteed, among other 
-- things, to be able to perform real division with its values.

-- ========================================================================== --
-- Classes and instances                                                      --
-- ========================================================================== -- 

-- Up to now we have seen how existing type classes appear in signatures such 
-- as:

-- (==) :: (Eq a) => a -> a -> Bool

--Now it is time to switch perspectives. First, we quote the definition of the
-- Eq class from Prelude:
{-
class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- Minimal complete definition:
    --
    (==) or (/=)
    x /= y
    = not (x == y)
    x == y
    = not (x /= y)
--}

-- The definition states that if a type a is to be made an instance of the class
-- Eq it must support:
-- the functions (==) and (/=) 
-- the class methods - both of them having type a -> a -> Bool. 

-- Additionally, the class provides default definitions for (==) and (/=) 
-- in terms of each other. 

-- As a consequence, there is no need for a type in Eq to provide both definitions
-- given one of them, the other will be generated automatically.

-- With a class defined, we proceed to make existing types instances of it. 
-- Here is an arbitrary example of an algebraic data type made into an instance 
-- of Eq by an instance declaration:

data Foo = Foo {x :: Integer, str :: String}

instance Eq Foo where
    (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

-- And now we can apply (==) and (/=) to Foo values in the usual way:    

-- Main> Foo 3 "orange" == Foo 6 "apple" 
-- False

-- Main> Foo 3 "orange" /= Foo 6 "apple"
-- True

-- A few important remarks:

-- * The class Eq is defined in the Standard Prelude. 
--   This code sample defines the type Foo and then declares it to be an 
--   instance of Eq. 
--   The three definitions (class, data type,and instance) are completely 
--   separate and there is no rule about how they are grouped.
--   This works both ways: you could just as easily create a new class Bar and
--   then declare the type Integer to be an instance of it.

-- * Classes are not types, but categories of types; and so the instances of a
--   class are types instead of values.

-- * The definition of (==) for Foo relies on the fact that the values of its
--   fields (namely Integer and String) are also members of Eq. In fact, almost
--   all types in Haskell are members of Eq (the most notable exception being 
--   functions).

-- Type synonyms defined with type keyword cannot be made instances of a class.


-- ========================================================================== --
-- Deriving                                                                   --
-- ========================================================================== --
-- Since equality tests between values are commonplace, in all likelihood most
-- of the data types you create in any real program should be members of Eq.
-- A lot of them will also be members of other Prelude classes such as Ord and
-- Show. To avoid large amounts of boilerplate for every new type, Haskell has a
-- convenient way to declare the "obvious" instance definitions using the keyword
-- deriving. 
-- So, Foo would be written as:


data Foo' = Foo' {fi :: Integer, fstr :: String}
    deriving (Eq, Ord, Show)

-- This makes Foo an instance of Eq with an automatically generated definition
-- of == exactly  equivalent to the one we just wrote, and also makes it an 
-- instance of Ord and Show for good measure.    

-- You can only use deriving with a limited set of built-in classes, which are
-- describedvery briefly below:

-- Eq
--  Equality operators == and /=

-- Ord
--  Comparison operators < <= > >=; min, max, and compare.

-- Enum
--  For enumerations only. Allows the use of list syntax such as 
--  [Blue .. Green].

-- Bounded
--  Also for enumerations, but can also be used on types that have only one
--  constructor. Provides minBound and maxBound as the lowest and highest values
--  that the type can take.

-- Show
--  Defines the function show, which converts a value into a string, and other related functions.

-- Read
--  Defines the function read, which parses a string into a value of the type, and other related
--  functions.


-- ========================================================================== --
-- Class inheritance                                                          --
-- ========================================================================== --

-- Classes can inherit from other classes. For example, here is the main part of
-- the definition of Ord in Prelude:
{--
    class (Eq a) => Ord a where
        compare              :: a -> a -> Ordering
        (<), (<=), (>=), (>) :: a -> a -> Bool
        max, min             :: a -> a -> a
--}


-- The actual definition is rather longer and includes default implementations 
-- for most of the functions. The point here is that Ord inherits from Eq. 
-- This is indicated by the => notation in the first line, which mirrors the way
-- classes appear in type signatures. Here, it means that for a type to be an 
-- instance of Ord it must also be an instance of Eq, and hence needs to 
-- implement the == and /= operations.

-- A class can inherit from several other classes: just put all of its 
-- superclasses in the parentheses before the =>. 
-- Let us illustrate that with yet another Prelude quote:
{--
class (Num a, Ord a) => Real a where
    -- | the rational equivalent of its real argument with full precision
    toRational
    :: a -> Rational
--}


-- ========================================================================== --
-- Type constraints                                                           --
-- ========================================================================== --

-- (+) :: (Num a) => a -> a -> a

-- (Num a) => is a type constraint, which restricts the type a to instances of
-- the class Num. 

-- Infact, (+) is a method of Num, along with quite a few other functions 
-- (notably, (*) and (-); but not (/)).

-- You can put several limits into a type signature like this:

foo :: (Num a, Show a, Show b) => a -> a -> b -> String
foo x y t =
    show x ++ " plus " ++ show y ++ " is " ++ show (x+y) ++ "." ++ show t


-- Here, the arguments x and y must be of the same type, and that type must be
-- an instance of both Num and Show. 

-- Furthermore, the final argument t must be of some (possibly different) type
-- that is also an instance of Show. 
-- This example also displays clearly how constraints propagate from the 
-- functions used in a definition (in this case, (+) and show) to the function 
-- being defined.    

-- A concerted example:

-- We will define a Located class, a Movable class which inherits from it, and
-- a function with a Movable constraint implemented using the methods of the 
-- parent class, i.e. Located.

-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a


-- An example type, with accompanying instances.
data NamedPoint = NamedPoint{
       pointName:: String,
       pointX:: Int, 
       pointY:: Int
} deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p 
    where
        (x, y) = getLocation p

