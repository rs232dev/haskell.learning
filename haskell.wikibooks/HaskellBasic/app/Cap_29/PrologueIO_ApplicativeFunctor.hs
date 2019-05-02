module Cap_29.PrologueIO_ApplicativeFunctor where

import Text.Read

-- ========================================================================== --
-- readMaybe                                                                  --
-- ========================================================================== --

interactiveDoubling = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Just x  -> putStrLn ("The double of your number is " ++ show (2*x))
        Nothing ->  do
            putStrLn "This is not a valid number. Retrying..." 
            interactiveDoubling    

-- Main Text.Read> interactiveDoubling
-- Choose a number:
-- 2
-- The double of your number is 4.0

-- Main Text.Read> interactiveDoubling
-- Choose a number:
-- hello
-- This is not a valid number. Retrying...
-- Choose a number:
-- 5
--The double of your number is 10.0


-- A variation of this solution might take advantage of how, given that Maybe
-- is a Functor, we can double the value before unwrapping mx in the case 
-- statement:

interactiveDoubling' = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case fmap (2*) mx of
        Just d  -> putStrLn ("The double of your number is " ++ show d)
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling'


-- Main Text.Read> interactiveDoubling'
-- Choose a number:
-- 5
-- The double of your number is 10.0


-- ========================================================================== --
-- Application in functors                                                    --
-- ========================================================================== --
-- Now, let's do something slightly more sophisticated:  reading two numbers 
-- with readMaybe and printing their sum (we suggest that you attempt writing 
-- this one as well before continuing).

interactiveSumming = do
    putStrLn "Choose two numbers:"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy
    case mx of
        Just x -> case my of
            Just y  -> putStrLn ("The sum of your numbers is " ++ show (x+y))
            Nothing -> retry 
        Nothing-> retry
        where
            retry = do
                putStrLn "Invalid number. Retrying..."
                interactiveSumming


-- Main Text.Read> interactiveSumming
-- Choose two numbers:
-- 1
-- 2
-- The sum of your numbers is 3.0

-- Main Text.Read> interactiveSumming
-- Choose two numbers:
-- hello
-- 2
-- Invalid number. Retrying...
-- Choose two numbers:


-- interactiveSumming works, but it is somewhat annoying to write. 
-- In particular, the nested case statements are not pretty, and make reading
-- the code a little difficult.

-- If only there was a way of summing the numbers before unwrapping them,
-- analogously to what we did with fmap in the second version of 
-- interactiveDoubling, we would be able to get away with just one case:

-- Wishful thinking...
-- case somehowSumMaybes mx my of
--  Just z  -> putStrLn ("The sum of your numbers is " ++ show z)
--  Nothing -> do
--      putStrLn "Invalid number. Retrying..."
--      interactiveSumming

-- But what should we put in place of somehowSumMaybes?
-- fmap, for one, is not enough.
-- While fmap (+) works just fine to partially apply (+) to the value wrapped
-- by Maybe

-- Main Text.Read> :t (+) 3
-- (+) 3 :: Num a => a -> a

-- Main Text.Read> :t fmap 
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- Main Text.Read> :t fmap (+) 
-- fmap (+) :: (Num a, Functor f) => f a -> f (a -> a)

-- Main Text.Read> :t fmap (+) (Just 3)
-- fmap (+) (Just 3) :: Num a => Maybe (a -> a)

-- we don't know how to apply a function wrapped in Maybe to the second value.
-- For that, we would need a function with a signature like this one.

-- (<*>) :: Maybe(a -> b) -> Maybe a -> Maybe b

-- Main Text.Read> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Main Text.Read> fmap (+) (Just 3) <*> Just 4
-- Just 7


-- (<*>) introduces us to a new type class:
-- 
-- Applicative, the type class of applicative functors.
-- For an initial explanation, we can say that an applicative functor is a
-- functor which supports applying functions within the functor, thus allowing
-- for smooth usage of partial application (and therefore functions of 
-- multiple arguments). 
-- All instances of Applicative are Functors, and besides Maybe, there are
-- many other common Functors which are also Applicative.

-- This is the Applicative instance for Maybe:
-- instance Applicative Maybe where
--  pure                = Just 
--  (Justf) <*> (Justx) = Just(f x)
--  _ <*> _             = Nothing

-- The definition of (<*>) is actually quite simple: 
-- if neither of the values are Nothing, apply the function f to x and wrap the
-- result with Just; otherwise, give back Nothing. 

-- Note that the logic is exactly equivalent to what the nested case statement of 
-- interactiveSumming does.

-- Note that beyond (<*>) there is a second method in the instance above, pure:
-- Main Text.Read> :t pure
-- pure :: Applicative f => a -> f a

-- pure takes a value and brings it into the functor in a default, trivial way. 
-- In the case of Maybe. 






