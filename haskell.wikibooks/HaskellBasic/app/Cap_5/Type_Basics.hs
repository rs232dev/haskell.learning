module Cap_5.Type_Basics where


-- ========================================================================== --
-- Haskell is STRONGLY TYPED                                                  --
-- ========================================================================== --
--
--
--    In Haskell, the rule is that all type names have to begin with a 
--    capital letter.    
--               |\___ 
--               |    \
myfun_double:: Int -> Int   -- signatures are placed just before the
                            -- corresponding function, for maximun
                            -- clarity

myfun_double x = x * 2       

{--  Output:
    
    Main> :t myfun_double 
    myfun_double :: Int -> Int
    
    Main> myfun_double 5
    25
--}        


-- Note that when a function have the same type we have the option of 
-- writing just one signature for all of them, by separating their names
-- with commas.

myfun_square, myfun_inc:: Int -> Int

myfun_square x = x * x
myfun_inc x = x + 1

{-- Output:

    Main> myfun_square 2
    4

    Main> myfun_inc 2
    3
--}




    
    






