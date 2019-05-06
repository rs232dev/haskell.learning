module Cap_31.MaybeMonad where

import Control.Monad

-- ========================================================================== --
-- Maybe Monad                                                                -- 
-- ========================================================================== --


safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0 = Just (log x)
    | otherwise = Nothing


-- Main> safeLog 1000
-- Just 6.907755278982137    

-- Main> safeLog (-1000)
-- Nothing

-- Left-to-right composition of Kleisli arrows:

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 



-- Right-to-left composition of Kleisli arrows. (>=>), with the arguments 
-- flipped:

-- (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

safeLog' = safeLog <=< safeLog

-- (*>) :: f a -> f b -> f b
-- Sequence actions, discarding the value of the first argument.



-- ========================================================================== --
-- Lookup tables                                                              -- 
-- ========================================================================== --

-- A lookup table relates keys to values. You look up a value by knowing its 
-- key and using the lookup table. For example, you might have a phone book 
-- application with a lookup table where contact names are keys to corresponding
-- phone numbers. An elementary way of implementing lookup tables in Haskell is 
-- to use a list of pairs: [(a, b)]. Here a is the type of the keys, and b the 
-- type of the values. 3 Here's how the phone book lookup table might look:

phonebook :: [(String, String)]
phonebook = [   ("Bob"  ,"01788 665242"),
                ("Fred" , "01624 556442"),
                ("Alice", "01889 985333"),
                ("John" , "91889 985333"),
                ("Mike" , "45410 115323"),
                ("Jane" , "01732 187565") 
            ]

-- The most common thing you might do with a lookup table is look up values.
-- Everything is fine if we try to look up "Bob", "Fred", "Alice" or "Jane" 
-- in our phone book, but what if we were to look up "Zoe"? Zoe isn't in our 
-- phone book, so the lookup would fail. Hence, the Haskell function to look 
-- up a value from the table is a Maybe computation (it is available from 
-- Prelude):
{-- 

lookup :: Eq a => a     -- a key
    -> [(a, b)]         -- the lookup table to use
    -> Maybe b          -- the result of the lookup

from: 

https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#lookup

lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

--}
   
-- Let us explore some of the results from lookup:

-- Main> lookup "Bob" phonebook
-- Just "01788 665242"

-- Main> lookup "Jane" phonebook
-- Just "01732 187565"

-- Main> lookup "Zoe" phonebook
-- Nothing

-- Now let's expand this into using the full power of the monadic interface.
--
-- Say, we're now working for the government, and once we have a phone number 
-- from our contact, we want to look up this phone number in a big, 
-- government-sized lookup table to find out the registration number of their 
-- car. This, of course, will be another Maybe-computation. 

-- But if the person we're looking for isn't in our phone book, we certainly 
-- won't be able to look up their registration number in the governmental 
-- database. 

-- What we need is a function that will take the results from the first 
-- computation and put it into the second lookup only if we get a successful
-- value in the first lookup. 
-- Of course, our final result should be Nothing if we get Nothing from either
-- of the lookups.

governmentDatabase :: [(String, String)]
governmentDatabase = [  ("01788 665242", "reg_01234 665242"),
                        ("01624 556442", "reg_01234 556442"),
                        ("01889 985333", "reg_45678 985333"),
                        ("01732 187565", "reg_98765 187565"),
                        ("45410 115323", "reg_32311 181534")
                    ]



getRegistrationNumber :: String             -- their name
                        -> Maybe String     -- their registration number

getRegistrationNumber name =
    lookup name phonebook >>=
        (\number -> lookup number governmentDatabase)


-- Main> getRegistrationNumber "Alice"
-- Just "r3_01889 985333"

-- Main> getRegistrationNumber "Paul"
-- Nothing

-- Main> getRegistrationNumber "John"
-- Nothing

taxDatabase :: [(String, String)]
taxDatabase =        [  ("reg_01234 665242", "200 usd"),
                        ("reg_01234 556442", "250 usd"),
                        ("reg_45678 985333", "350 usd"),
                        ("reg_98765 187565", "400 usd") 
                      ]


getTaxValue name =
    lookup name phonebook >>=
        (\number -> lookup number governmentDatabase) >>=
            (\number -> lookup number taxDatabase)

-- Main> getTaxValue "Alice"
-- Just "350 usd"

-- Main> getTaxValue "John"
-- Nothing

-- Main> getTaxValue "Mike"
-- Nothing

-- Let's just pause here and think about what would happen if we got a Nothing
-- anywhere. 
-- By definition, when the first argument to >>= is Nothing, it just returns 
-- Nothing while ignoring whatever function it is given. 
-- Thus, a Nothing at any stage in the large computation will result in a 
-- Nothing overall, regardless of the other functions. After the first Nothing
-- hits, all >>=s will just pass it to each other, skipping the other function
-- arguments. 
-- The technical description says that the structure of the Maybe monad 
-- propagates failures.

-- If we have a Just value, we can extract the underlying value it contains
-- through pattern matching.

zeroAsDefault :: Maybe Int -> Int
zeroAsDefault mx = case mx of
    Nothing -> 0
    Just x  -> x

-- Main> zeroAsDefault Nothing
-- 0

-- Main> zeroAsDefault (Just 2)
-- 2



