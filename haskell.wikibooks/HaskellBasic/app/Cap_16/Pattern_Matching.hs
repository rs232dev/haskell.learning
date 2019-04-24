module Cap_16.Pattern_Matching where




data Foo = Bar | Baz Int
-- -------------------------------------------------------------------------- --
-- Here Bar and Baz are constructors for the type Foo. You can use them for   --
-- pattern matching Foo values and bind variables to the Int value contained  --
-- in a Foo constructed with Baz:                                             --
-- -------------------------------------------------------------------------- --
-- Main> :t Bar
-- Bar :: Foo

-- Main> :t Baz
-- Baz :: Int -> Foo

f0 :: Foo -> Int
f0 Bar = 1 
f0 (Baz x) = x - 1

var1 = Bar
var2 = Baz 5

-- Main> f0 var1
-- 1

-- Main> f0 var2
-- 4

f1:: [a] -> String
f1 []     = "empty list"
f1 (x:xs) = "not empty list"

-- Main> f1 []
-- "empty list"

-- Main> f1 [1,2,3]
-- "not empty list"

-- ========================================================================== --
-- Tuple constructors                                                         --
--                                                                            --
-- Analogous considerations are valid for tuples. Our access to their         --
-- components via pattern matching...                                         --
-- ========================================================================== --
fstPlusSnd :: (Num a) => (a, a) -> a
fstPlusSnd (x, y) = x + y

-- ========================================================================== --
--- Matching literal values                                                   --
-- ========================================================================== --

f :: Int -> Int
f 0 = 1
f 1 = 5
f 2 = 2
f _ = -1

-- is performing pattern matching as well, matching the argument of
-- f with the Int literals 0, 1 and 2, and finally with _

-- ...or with constructor patterns. For instance, this function
g :: [Int] -> Bool

g (0:[]) = False
g (0:xs) = True
g _ = False
