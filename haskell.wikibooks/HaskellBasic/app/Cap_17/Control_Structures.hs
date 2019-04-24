module Cap_17.Control_Structures where

-- ========================================================================== --
-- if and guards revisited                                                    --
--                                                                            --
-- if <condition> then <true-value> else <false-value>                        --
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
