module Cap_12.SimpleIO where

-- -------------------------------------------------------------------------- --
-- Back to the real world                                                     --
-- -------------------------------------------------------------------------- --
my_ioaction :: IO ()
my_ioaction =  putStrLn "IO values actions";   



-- -------------------------------------------------------------------------- --
-- do notation                                                                --
-- -------------------------------------------------------------------------- --
  
--   do notation provides a convenient means of putting actions together 
--   (which is essential in doing useful things with Haskell).

my_ioaction' :: IO ()
my_ioaction' = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", how are you?")

