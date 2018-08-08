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
--
-- -------------------------------------------------------------------------- --
--     ___  ____    _  __     __       __  _         
--    / _ \/ __ \  / |/ /__  / /____ _/ /_(_)__  ___ 
--   / // / /_/ / /    / _ \/ __/ _ `/ __/ / _ \/ _ \
--  /____/\____/ /_/|_/\___/\__/\_,_/\__/_/\___/_//_/
-- -------------------------------------------------------------------------- --
--     
-- Haskell provides a special notation for expression of the above form, 
-- allowing them to be written in a simple manner as follows:
-- do x1 <- m1
--    x2 <- m2
--    ...
--    ...
--    ...
--    xn <- mn
--    f x1 x2 ... xn

-- in this case each item in the sequence must begin in the same column, and
-- Using this notation (do notation), meval can now be redefined as:
doeval :: Expr -> Maybe Int
doeval (Val n)   = Just n
doeval (Div x y) = do n <- doeval x
                      m <- doeval y
                      safediv n m

-- examples:
-- > GH_course2.doeval (GH_course2.Div (GH_course2.Val 1) (GH_course2.Val 0))
-- Nothing
--
-- > GH_course2.doeval (GH_course2.Div (GH_course2.Val 0) (GH_course2.Val 1))
-- Just 0
-- 
-- > GH_course2.doeval (GH_course2.Div (GH_course2.Val 8) (GH_course2.Val 2))
-- Just 4

-- do notation is not specific to the type IO and Maybe, but can be used with 
-- any aoolicative type that forms a monad.
-- 
-- In Haskell, the concept of monad is captured by the following builtin
-- declaration:
--
-- class Applicative m => Monad m where
--    return ::   a                 -> m a
--    (>>=)  :: m a -> (  a -> m b) -> m b
--
-- return = pure
--
-- A monad is an applicative type m that supports return and >>= functions of 
-- the specified types.
-- The default definition return = pure that return is normally just another
-- name for the applicative functor pure.
-- 

-- Maybe Monad
--
-- instance  Monad Maybe  where
-- -- (>>=) ;;  Maybe a -> (a -> Maybe b) -> Maybe b      
--    Nothing  >>= _      = Nothing
--    (Just x) >>= f      = f x

-- List Monad
--
-- instance  Monad []  where
-- -- (>>=) ::  [a] -> (a -> [b]) -> [b]      
--    xs >>= f = [y | x <- xs, y <- f x]
--
-- xs >>= f applies the function f to each of the result in the list xs,
-- collecting all the resulting values in a list.
-- In this manner the bind operator (>>=) for list provides a means of 
-- sequencing expressions that may produce multiple results.
-- -------------------------------------------------------------------------- --
--
-- example (do notation):
pairs :: [a] -> [b] -> [(a,b)]
pairs [] _  = []
pairs _ []  = []
pairs xs ys = do x <- xs
                 y <- ys
                 return  (x,y)
--
-- > pairs [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]
-- -------------------------------------------------------------------------- --
--
-- example (comprehension notation):
pairs' :: [a] -> [b] -> [(a,b)]
pairs' [] _  = []
pairs' _ []  = []
pairs' xs ys = [(x,y) | x <- xs, y <- ys]

-- > pairs' [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- -------------------------------------------------------------------------- --


-- ========================================================================== == 
--    ______       __        __  ___                  __                      ==
--   / __/ /____ _/ /____   /  |/  /__  ___  ___ ____/ /                      ==
--  _\ \/ __/ _ `/ __/ -_) / /|_/ / _ \/ _ \/ _ `/ _  /                       ==
-- /___/\__/\_,_/\__/\__/ /_/  /_/\___/_//_/\_,_/\_,_/                        ==
--                                                                            ==    
-- ========================================================================== ==
--
-- Consider function that manipulate some form of state that can be changed over    
-- time.
-- For simplicity, we assume that the state is just an integer value, but this
-- can be modified as required.
--
type State = Int

-- The most basic form of function on this type is a state transformer ST, which
-- takes in inpute state as its argument and produces an output state as its 
-- result, i which the output state reflects any updates that were made to the 
-- state by the function during its execution.

-- type ST = State -> State

-- In general homewever we may wish to return a result value in addition to
-- updating the state. For example, if the state  represents a counter, a 
-- function for incrementing the counter may also wish to return its current
-- value.
-- For this reason we generalise the type of the state transformer ST to:

--type ST a = State -> (a, State)

--
--               ---------------            output
--               |              | -----> v (value)
--               |              |
--               |              |
--  input        |              |
-- current state |              |             output
--       s --->  |              | -----> s' (new state)
--               ----------------

-- Given that ST is a parameterised type, it is natural to try make it into a
-- monad, so that the do notation cen then be used to write stateful programs.
-- Homewever, types declared using type mechanism cannot be made into instances
-- of classes.
-- Hence, we first redefine ST using the newtype mechanism, whuch requires 
-- introducing a dummy constructor, wich we call S:

newtype ST a = S (State -> (a,State))

-- It's also convenient to define a special purpose application function for
-- this type, which simply remove the dummy constructor:
--
app :: ST a -> State -> (a, State)
app (S st) x =  st x

-- test:
-- > nnn = S(\x -> (1,x))
