module GH_course2 where 


-- ========================================================================== ==    
--     __  ___                  __                                            ==
--    /  |/  /__  ___  ___ ____/ /__                                          ==
--   / /|_/ / _ \/ _ \/ _ `/ _  (_-<                                          ==
--  /_/  /_/\___/_//_/\_,_/\_,_/___/                                          ==
-- ========================================================================== ==

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)    = n

-- this function  does not take in account of the possibility of division by 
-- zero, and will produce an error:
eval (Div x y)  = eval x `div` eval y

-- example
-- GH_course2.eval (GH_course2.Div (GH_course2.Val 1) (GH_course2.Val 0))
-- *** Exception: divide by zero

-- in order to address this, we can use Maybe type to define a safe version:
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- now we modify the evaluator
eval' :: Expr -> Maybe Int
eval' (Val n)    = Just(n)
eval' (Div x y)  = case eval' x of 
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                Nothing -> Nothing
                                Just m -> safediv n m

-- example:
-- GH_course2.eval' (GH_course2.Div (GH_course2.Val 1) (GH_course2.Val 0))
-- Nothing

-- the new definition for eval' resolve the dividion by zero issue, but is 
-- rather verbose.
-- To simplify the definition, we might use the fact that Maybe is applicative
-- and attempt to redefine eval' in applicative style:
--
-- eval'' :: Expr -> Maybe Int
-- eval'' (Val n)   = pure n

-- eval'' (Div x y) = pure safediv <*> eval'' x <*> eval'' y

-- the above definition is not type correct:
-- Couldn't match type ‘Maybe Int’ with ‘Int’
-- Expected type: Expr -> f Int
--  Actual type: Expr -> f (Maybe Int)
-- eval'' (Div x y) = pure safediv <*> eval'' x <*> eval'' y

-- In particular, the funcction safediv has type Int -> Int -> Maybe Int,whereas
-- in the above context a function of type Int -> Int -> Int is required:
--
-- :t (pure GH_course2.safediv)
-- (pure GH_course2.safediv)
--       :: Applicative f => f (Int -> Int -> Maybe Int)
--
-- The conclusion is that the eval'' function does not fit the pattern of
-- effectful programming that is capture by applicative functor.
-- 
-- The applicative style restrict us to applying pure function to effectful
-- arguments.
-- eval'' does not fit this pattern because the function safediv is not a
-- pure function, but may itsefl fail.

-- How can rewrite safediv in simple manner?
-- The keynis to observe the common pattern that occurs twice in its 
-- definition. 
-- A case analysis on a Maybe value, mapping Nothing to itself and Just x to
-- some result depending on x.
-- Abstracting out this pattern gives a new operator >>= (bind) that is
-- defined as follow:
--
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--
-- Prelude definition:
--
-- ::t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- mx >>= f = case mx of
--            Nothing -> Nothing
--            Just x -> f x

-- The >>= operator takes an argument of type a that may fail and a function
-- of type a -> b whose result may fail, and returns a resutl of type b that may
-- fail.

-- If the arguments fails we propagate the failure, otherwise we apply the 
-- function to the resulting value.
-- The >>= operator is often called bind, because the second argument binds the
-- result of the first.

meval :: Expr -> Maybe Int
meval (Val n)    = Just n
meval (Div x y)  = meval x >>= \n -> 
                   meval y >>= \m ->
                    safediv n m

-- examples:
--
-- :GH_course2.meval (GH_course2.Div (GH_course2.Val 1) (GH_course2.Val 0))
-- Nothing
-- 
-- :GH_course2.meval (GH_course2.Div (GH_course2.Val 1) (GH_course2.Val 2))
-- Just 0
--
-- :GH_course2.meval (GH_course2.Div (GH_course2.Val 4) (GH_course2.Val 2))
-- Just 2



-- The cae for division states that we first evaluate x and call its result 
-- value n, then evaluating y and call its result value m, and finally combine
-- the two results by applying safediv.
-- This function can also be written in a single line:
--
-- meval (Div x y)  = meval x >>= \n -> meval y >>= \m -> safediv n m

-- The definition of the >>= operator ensures that such an expression only
-- succeds if every component in the sequence succeds.
-- Moreover the user does nothave to worry about dealing with the possibility    
-- of failure at any point of sequence, as this handled automatically by the
-- definition of the >>= operator.
