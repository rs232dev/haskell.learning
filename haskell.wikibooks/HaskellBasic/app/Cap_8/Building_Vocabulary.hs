module Cap_8.Building_Vocabulary where


-- -------------------------------------------------------------------------
-- Function Composition                                                   --
-- -------------------------------------------------------------------------

add2:: Int -> Int
add2 x = x + 2

mult2:: Int -> Int
mult2 x = x * 2  

my_comp:: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
my_comp f g = f . g

-- Main> (my_comp add2 mult2) 5
-- 12
