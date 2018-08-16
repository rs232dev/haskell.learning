module Computerwhile where 


data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y


safediv :: Int -> Int -> Maybe Int
safediv n m = 
                if m == 0 then Nothing
                else
                    Just( n `div` m)


eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n -> case eval' y of
                                Nothing -> Nothing
                                Just m -> safediv n m




eval'' :: Expr -> Maybe Int
eval'' (Val n)   = return n
eval'' (Div x y) = eval'' x >>= 
                                (\n -> 
                                    eval'' y >>= (\m -> safediv n m)
                                )    
    



                                