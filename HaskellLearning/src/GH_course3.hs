module GH_course3 where 

newtype State s a = State { runState :: s -> (a, s) }

-- we're implicitly defining a function to extract our inner function 
-- from the data type: that function is called runState. 
-- data State s a = State { runState :: s -> (a, s) }

-- > :t runState
-- runState :: State s a -> s -> (a, s)

-- runState is just a way to unwrapping data constructor State.

--     partial apply of the data constructor State            
--                 |   
instance Monad (State s) where
    -- return :: a -> State s a
    return a = State (\s -> (a,s)) -- or  return a = State $ \s -> (a,s)

    -- f :: s -> (a,s)
    -- k :: a -> State s b

    -- >>= :: State s a -> (a -> State s b) -> State s b
    State f >>= k = State ( \s -> let (a,s') = f s 
                                    in runState (k a) s')
       
    -- k a               :: State s b
    -- runState (k a)    :: s -> (b,s)
    -- runState (k a) s' :: (b,s)

instance (Show s,Show a) => Show (State s a) where
        show (State a) =  "asd"

instance Functor (State s) where
    fmap f (State g) = State (\st -> let (x, st') = g st 
                                         in (f x, st'))
    

instance Applicative (State s) where
    pure x    = State $ \s0 -> (x, s0)
    fs <*> xs = State $ \s0 -> let (f, s1) = runState fs s0
                                   (x, s2) = runState xs s1
                               in  (f x, s2)

-- The $ operator is for avoiding parentheses. Anything appearing after it will 
-- take precedence over anything that comes before.

-- It's type is s -> (a,s). 
-- Essentially, it's a type for any function that takes some initial state s 
-- and then returns a tuple of (regular return value, new state). 
-- That makes sense. And because of partial applications, currying, etc. we can
-- actually write something like the following: 



evalState :: State s a -> s -> a
evalState act = fst . runState act



--data State s a = State (s -> (a, s) )

-- First of all the State monad is just an abstraction for a function that takes
-- a state and returns an intermediate value and some new state value.
--
-- What we have is the State constructor acting as a container for a 
-- function :: s -> (a,s), while the definition for bind just provides a 
-- mechanism for "composing" a function state -> (val,state) within the State 
-- wrapper.

-- Here’s a stupid example of a function that can be "contained" in our state
-- type:

-- look at our counter and return "foo" or "bar"  along with the incremented 
--counter:  
--
fromStoAandS :: Int -> (String,Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
               | otherwise = ("bar",c+1)

-- example:
-- > fromStoAandS  25
-- ("foo",26)
-- 
-- > fromStoAandS  19
-- ("bar",20)

-- If we just wrap that in a State constructor, we’re in the State monad:
--
--                State  s    a
stateIntString :: State Int String
stateIntString = State (\x -> ("some work", x+1))
--stateIntString = State fromStoAandS

stateIntString2 :: State Int String
stateIntString2 = State (\x -> ("some work2", x+10))

boh :: State Int ()
boh = State (\x -> ((), x+10))

-- But what about runState? All that does of course is give us the "contents" of
-- our State constructor: 
--
-- i.e. a single function :: s -> (a,s). 
--
-- It could have been named stateFunction but someone thought it would be really 
-- clever to be able to write things like:


--           (State s a) = State { runState :: s -> (a, s) }
--                 /
--                /
-- > runState stateIntString 5
-- ("foo",6)
--
-- > runState stateIntString 1
-- ("bar",2)









-- > :t GH_course3.State
-- GH_course3.State :: (s -> (a, s)) -> GH_course3.State s a

-- mystate a = State (\st -> (a,st))


-- > :t mystate
-- mystate :: a -> GH_course3.State s a

{--
stx f = 
        \st -> let (x, st') = runState st 
                    in (f x, st')

                    --}
-- > :t stx
-- stx :: (t -> a) -> (p -> (t, b)) -> p -> (a, b)

-- where:
--
-- f        :: (t -> a)
-- st       :: p
-- runState :: p -> (t, b)
-- f x      :: a
-- st'      :: b
-- 
-- the  let binding in the above function works as below indicated;
-- 
-- The tuple (x, st') is bound with the function application of runState to the
-- st parameter:
--          (x, st')
--            \   \
--             \   \ _ new state
--              \
--               \_ value
--
-- g   :: p -> (t, b)
--
-- then:
--
--  (f x, st')
--
-- produces a tuple with the result of the f x application (new value) and 
-- replies the new state


{--

First of all the State monad is just an abstraction for a function that takes a
state and returns an intermediate value and some new state value.

What we have is the State constructor acting as a container for a 

function :: s -> (a,s), while the definition for bind just provides a mechanism 
for "composing" a function state -> (val,state) within the State wrapper.

But what about runState? All that does of course is give us the “contents” of our
State constructor: 
i.e. a single 

    function :: s -> (a,s). 

--}

--newtype State'' s a = State { runState :: s -> (a, s) }

-- runState :: State'' s a -> s -> (a,s) 


{--
newtype State' s a = State (s -> (a, s))

instance Functor (State' s) where
    fmap f (State g) = State (\st -> let (x, st') = g st 
                                     in (f x, st'))

data State'' s a = State' (s -> a) (s -> s)                                     

--}

-- data State' s a = State' (s -> a) (s -> s)
-- runState' (State' f tr) s = (f s, tr s)


data Fc    x z = Build { runSt     :: x -> (x,z)  }
--data Fc    x z = Build ( x -> (x,z)  )

-- > :t runSt
-- runSt :: Fc x z -> x -> (x, z)

test::Int -> (Int,Int)
test x = (1,x+1)

func::Fc Int Int
func = Build test

func'::Fc Int Int
func' = Build (\x -> (x,x +10))

--   runSt :: Fc x z -> x -> (x, z)
--            ------    -
--              |       |
-- > runSt   func       1
--
-- (1,2)

{-- 
func1 :: Int -> (String,Int)
func1 c   | c `mod` 5 == 0 = ("foo",c+1)
          | otherwise = ("bar",c+1)


func2 :: Int -> (String,Int)
func2 c   | (<=) c 10 = ("less or equal to 10 ",c+1)
          | otherwise = ("greate than 10",      c+50)

--}          

func0 :: Int -> (String,Int)
func0 x = ("some work.0 (+10)", x+10)

func1 :: Int -> (String,Int)
func1 x = ("some work.1 (+100)", x+100)

func2 :: Int -> (String,Int)
func2 x = ("some work.1 (+1000)", x+1000)

--         State  s    a
myState :: State Int String
myState = State (\x -> ("some work2", x+10))

funcz :: Int -> (String,Int)
funcz x =  do            
            ("some work.0 (+10)", x+10)


-- > tx = State (\x -> ("ciao",x+1)) >>= (\z -> State (\x -> (z++".. some work2", x+10)))
-- > runState tx 4
-- ("ciao.. some work2",15)


{--

λ:tx = myState >>= (\x -> myState)
λ:runState tx 3
("some work2",23)
λ:tx = myState >>= (\x -> myState) >>= (\x -> myState)
λ:runState tx 3
("some work2",33)
λ:

myState_1 :: GH_course3.State Int String
λ:tx = myState_1 >>= (\x -> myState_2) >>= (\z -> State (\x -> ("some work2", x+10)))
λ:runState tx 50
("some work2",111)
λ::t tx
tx :: GH_course3.State Int [Char]
λ:


--}


newtype StateIO s a = StateIO { runStateIO :: s -> (a, s) }

funcIO0 :: Int -> (IO (),Int)
funcIO0 x = do 
                (putStrLn("ciao"), x+10)


funcIO1 :: Int -> (IO (),Int)
funcIO1 x = do 
                (putStrLn("hello"), x+100)


instance Show a => Show (StateIO s a) where
    show (StateIO s) = "T " ++ show ""


type Memory = [(String, String)]

varLookUp :: String -> Memory -> (String, Memory)
varLookUp name mem = case varLookUpList' name mem of
                            (Just s) -> (s, mem)
                            Nothing -> ("Not found", mem)
    where
        varLookUpList' :: String -> Memory -> Maybe String
        varLookUpList' name [] = Nothing
        varLookUpList' name ((n,v):xs) = if name == n 
                                             then Just v 
                                             else varLookUpList' name xs



                                             

-- -------------------------------------------------------------------------- --
--   State Monad laws:                                                        --
-- -------------------------------------------------------------------------- --

-- Left identity:

-- Monad law 1 states:

  -- given: x :: a  and  g :: a -> State s b
  -- return x >>= g  ==  g x

  -- given: mv :: State s a  and  return :: a -> State s a
  -- mv >>= return  ==  mv


  -- Monad law 3 
  -- This derivation is going to be a bit painful. If you skip it, you won't hurt my feelings. But anyway, here we go.

  -- Recall the definition of monad law 3, in terms of the >>= operator:
  
--    (mv >>= f) >>= g  ==  mv >>= (\x -> (f x >>= g))
  
