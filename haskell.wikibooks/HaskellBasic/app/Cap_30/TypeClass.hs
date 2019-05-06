module Cap_30.TypeClass where


-- ========================================================================== --
-- Type Class                                                                 --
-- ========================================================================== --

-- In Haskell, the Monad type class is used to implement monads. 
-- It is provided by the Control.Monad module and included in the Prelude. 

-- The class has the following methods:
{--

    class Applicative m => Monad m where
        return :: a      -> m a
        (>>=)  :: m a    -> (a -> m b) -> m b
        (>>)   :: m a    -> m b -> m b
        fail   :: String -> m a



The operator (>>), spelled "then", is a mere convenience and has the default 
implementation:

    m >> n = m >>= \_ -> n


(>>) sequences two monadic actions when the second action does not involve the
result of the first, which is a common scenario for monads such as IO.

--}

printSomethingTwice :: String -> IO ()
printSomethingTwice str = putStrLn str >> putStrLn str

-- Main> printSomethingTwice "hello"
-- hello
-- hello
















