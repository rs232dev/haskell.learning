module Cap_7.Type_BasicsII where


    -- -------------------------------------------------------------------------
    -- The Num class                                                      --
    -- -------------------------------------------------------------------------
    {-- 
        In order to capture such generality in the simplest way possible we
        would like to have a very general Number type in Haskell.

        Num is a typeclass - a group of types which includes all types which
        are regarded as numbers.
        The Num a => parts of a signature restrict a to number types, or more
        accurately, instance of Num.

        -- from Prelude:
        -- Main> :t (+)
        --(+) :: Num a => a -> a -> a

    --}

    {-- 
        Int:
            corresponds to the plain integer type found in most languages. 
            It has fixed maximum and minimum values that depend on a computer's 
            processor. (In 32-bit machines the range goes from 
            -2147483648 to 2147483647).

        Integer:
            also is used for integer numbers, but it supports arbitrarily large
            values – at the cost of some efficiency.

        Double:
            is the double-precision floating point type, a good choice for real
            numbers in the vast majority of cases. (Haskell also has Float, the 
            single-precision counterpart of Double, which is usually less 
            attractive due to further loss of precision.

        Several other number types are available, but these cover most in 
        everyday tasks.    
    --}
    
    my_integral_division:: (Fractional a) => a -> a -> a
    my_integral_division x y =  (x/y)

    
