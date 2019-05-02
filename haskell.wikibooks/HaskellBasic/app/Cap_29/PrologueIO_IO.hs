module Cap_29.PrologueIO_IO where

import Text.Read

-- ========================================================================== --
-- IO                                                                         --
-- ========================================================================== --

addExclamation :: String-> String
addExclamation s = s ++ "!"

test_addExclamation = putStrLn (addExclamation "Hello")


test_addExclamation' = putStrLn ("Hello" ++ "!")

