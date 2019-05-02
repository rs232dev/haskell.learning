module Cap_29.PrologueIO_IO where

import Text.Read

-- ========================================================================== --
-- IO                                                                         --
-- ========================================================================== --

addExclamation :: String-> String
addExclamation s = s ++ "!"

test_addExclamation = putStrLn (addExclamation "Hello")

test_addExclamation' = putStrLn ("Hello" ++ "!")


interactiveSumming' :: IO()
interactiveSumming' = do
    putStrLn "Choose two numbers:"
    mx <- readMaybe <$> getLine  -- equivalently: fmap readMaybe getLine
    my <- readMaybe <$> getLine
    case (+) <$> mx <*> my :: Maybe Double of
        Just z  -> putStrLn ("The sum of your numbers is " ++ show z)
        Nothing -> do
            putStrLn "Invalid number. Retrying..."
            interactiveSumming'


-- readMaybe :: Read a => String -> Maybe a
            
-- readMaybe <$> getLine can be read as "once getLine delivers a string, 
-- whatever it turns out to be, apply readMaybe on it". 

-- (<$>) is merely an infix synonym for fmap, so you can write e.g.
--
-- Main Text.Read> (*2) <$> [1..3]
-- [2,4,6]
-- 
-- Main Text.Read> fmap (*2) [1..3]
-- [2,4,6]

-- How interactiveSumming' works:
--
-- 1) putStrLn "Choose two numbers:" ==> print a String to stdout
-- 2) - mx <- readMaybe <$> getLine  -- equivalently: fmap readMaybe getLine
-- 3) - my <- readMaybe <$> getLine  -- equivalently: fmap readMaybe getLine
--  
-- the steps 2,3 read a String by getLine that returns:
--
--       getLine :: IO String
--
-- then apply:
--              fmap readMaybe getLine
--
-- readMaybe :: Read a => String -> Maybe a
--
-- fmap :: Functor f => (a -> b) -> f a -> f b
--                        /           \      \_____ IO (Maybe a)    
--                       /             \
--                    readMaybe       getLine
--                ::String -> Maybe a     ::IO String  
--
-- that is the same as below:
--
--
--                  readMaybe :: Read a => String -> Maybe a
--                       /________________/
--                            /
--                           /
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
--                                   /       \                               
--                                  /         \
--                      getLine::IO(String)    \____ IO ( Maybe a)
--                                f    a             f         b
--
-- by the do notation is possible to get the encapsulated values IO (Maybe a)
-- then mx and my will be two types of: Maybe a 

-- 4) case (+) <$> mx <*> my :: Maybe Double of
--
-- The above code can be explained  as below:

-- 4.1)  (+) <$> mx  corresponding to:
--
--     fmap (+) (Maybe a)
--
-- example
-- Main Text.Read> f1 = (fmap (+) (Just 7)) 
-- Main Text.Read> :t f1
-- f1 :: Num a => Maybe (a -> a)
--
-- then we apply the wrapped function Maybe (a -> a) by <*>:
-- 
-- Main Text.Read> :t f1
-- f1 :: Num a => Maybe (a -> a)
-- Main Text.Read> f1 <*> Just(2)
-- Just 9


-- some examples:
my_example = do
    a1 <- readMaybe <$> getLine 
    case a1::Maybe Integer of
        Just x  ->  putStrLn ("value:" ++ show x)
        Nothing ->  putStrLn ("empty")

my_example' = do
    a1 <- fmap readMaybe getLine 
    case a1::Maybe Integer of
        Just x  ->  putStrLn ("value:" ++ show x)
        Nothing ->  putStrLn ("empty")





