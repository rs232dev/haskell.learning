module Cap_3_4.Variables_and_Functions where 

myvar = 2         -- variable
myfun x  = x + 1  -- function
    
-- ========================================================================== --
-- local definition                                                           --
-- ========================================================================== --
{-- 
    When defining a function, it's not uncommon to define intermediate 
    result that are local to the function.        
--}
myfun2 x =  (s + x) * s
    where s = x + 2

-- ========================================================================== --
-- Prefix Infix operators                                                     --
-- ========================================================================== --

prefix_op x y = (+) x y
infix_op x y = x + y

{-- Output:
    
    Main> prefix_op 1 2
    3

    Main> infix_op 1 2
    3
--}

-- ========================================================================== --
-- Boolean operations                                                         --
-- ========================================================================== --
boolean_and  x y = (x < y) && (y <10)
boolean_or   x y = (x < y) || (y <10)
boolean_not  x y = not ((x < y) || (y <10))

-- ========================================================================== --
-- Guards                                                                     --
-- ========================================================================== --
myabs x
    | x < 0 = 0 - x
    | otherwise = x

{-- Output:

    Main> myabs 9
    9

    Main> myabs (-9)
    9
--}


numOfSolutions a b c
    | disc > 0     = 2
    | disc == 0    = 1
    | otherwise    = 0
        where
            disc = b^2 - 4*a*c


        







