module StateMonad where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)    

newtype State s a = State (s -> (a, s))

runState :: State s a -> s  -> (a, s)
runState    (State st)   s' =   st s'

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap    

instance Monad (State s) where
    --  return    :: (Monad m) => a -> State a	
    return a  =  State (\s -> (a, s))

    --               __ m__  _a_         _ (a -> m b) _          _ m _  _ b_
    --              /      /    /       /              /       /      /    /
    --  (>>=)    :: State s  a     ->  (a -> State s b)    ->  State s  b
    m >>= k  = State  (\s -> let (a, s') = runState m s
                                                 in runState (k a) s')   


elab  = State (\x -> ("do", x+1)) >>= 
        \y -> State(\x -> (y++" ...some stuff!",x+10))

  

