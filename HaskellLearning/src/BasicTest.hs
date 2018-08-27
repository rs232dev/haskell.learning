module BasicTest where

    newtype CoType a = CoType(Int -> (a,Int))

    cotest :: CoType a -> Int -> (a,Int)
    cotest (CoType a) inp = a inp


    co1 =  CoType (\x -> ("value", x+1))
