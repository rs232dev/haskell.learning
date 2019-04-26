module Cap_17.Control_Structures where

-- ========================================================================== --
-- if <condition> then <true-value> else <false-value>                        --
--                                                                            --
-- ========================================================================== --
describeLetter :: Char -> String
describeLetter c = 
    if c >= 'a' && c <= 'z'
        then
            "Lower case"
        else if
            c >= 'A' && c <= 'Z'
        then "Upper case"
        else "Not an ASCII letter"

-- Main> describeLetter 'A'
-- "Upper case"

-- Main> describeLetter 'a'
-- "Lower case"


-- ========================================================================== --
-- Guards                                                                     --
-- ========================================================================== --
describeLetter' :: Char -> String
describeLetter' c
    | c >= 'a' && c <= 'z' = "Lower case"
    | c >= 'A' && c <= 'Z' = "Upper case"
    | otherwise            = "Not an ASCII letter"
    
--Main> describeLetter' 'B'
-- "Upper case"

-- Main> describeLetter' 'b'
-- "Lower case"

-- Main> describeLetter' '1'
-- "Not an ASCII letter"
    
-- ========================================================================== --
-- Embedding if expressions                                                   --
-- ========================================================================== --
funA x y = (if x == 0 then 1 else sin x / x) * y

-- ========================================================================== --
-- case expressions                                                           --
-- ========================================================================== --

-- Take this simple piece-wise definition:

funB:: Int -> Int
funB 0 = 18
funB 1 = 15
funB 2 = 12
funB x = 12 - x

funC:: Int -> Int
funC x =
    case x of
        0 -> 18
        1 -> 15
        2 -> 12
        _ -> 12 - x

-- Main> funC 1
-- 15

-- Main> funC 10
-- 2

-- ========================================================================== --
-- Controlling actions, revisited                                             --
-- ========================================================================== --
doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    if (read guess) < num
        then do putStrLn "Too low!" 
                doGuessing num
        else if (read guess) > num
            then do putStrLn "Too high!"
                    doGuessing num
            else do putStrLn "You Win!"

doGuessing' num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    case compare (read guess) num of
        LT -> do putStrLn "Too low!"
                 doGuessing' num
        GT -> do putStrLn "Too high!"
                 doGuessing' num
        EQ -> putStrLn "You Win!"

-- Main> doGuessing' 4

-- Enter your guess:
-- 5
-- Too high!

--Enter your guess:
-- 3
-- Too low!

-- Enter your guess:
-- 4
-- You Win!

