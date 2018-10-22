
module WriterMonad where


import Control.Monad.Writer
    
    
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x+1, ["Got number: " ++ show x])  -- here

-- or can use a do-block to do the same thing, and clearly separate the logging from the value
logNumber2 :: Int -> Writer [String] Int  
logNumber2 x = do
    tell ["Got number: " ++ show x]
    return x            
      
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5
    tell ["multiplying " ++ show a ++ " and " ++ show b ]
    return (a*b)


-- λ:print $ runWriter multWithLog
-- (15,["Got number: 3","Got number: 5","multiplying 3 and 5"])

-- λ:print $ runWriter (logNumber 1 >>= logNumber)
-- (3,["Got number: 1","Got number: 2"])


{--

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w) <*> Writer (a, w') = Writer (f a, w <> w')

instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w) >>= f = let (b, w') = runWriter (f a)
                        in Writer (b, w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)

--}