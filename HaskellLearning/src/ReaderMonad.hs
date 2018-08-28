module ReaderMonad where

import Control.Monad.Reader



-- In Haskell, our code is generally "pure", meaning functions can only 
-- interact with the arguments passed to them. 
-- This effectively means we cannot have global variables. We can have global
-- expressions, but these are fixed at compile time. 
-- If user behavior might change them, we have to wrap them in the IO monad, 
-- which means they can't be used from pure code.


-- ========================================================================== --
-- Example of Monad Reader (loadEnv from file)                                --
-- ========================================================================== --
func1 :: Reader String String
func1 = do
    env <- ask    
    res <- func2
    let res1 = env    
    return (res1 ++" - Result: " ++ (show res))

func2 :: Reader String Int
func2 = do    
    env <- ask    
    let res3 = read env
    return (2+res3)

-- loadEnv :: String -> IO String
-- loadEnv s = readFile s

-- ========================================================================== --
-- Example of Monad Reader (loadEnv' from Environment Record type)            --
-- ========================================================================== --

data Environment = Environment
  { param1 :: String
  , param2 :: String
  , param3 :: String } deriving (Show)  

get_param1 :: Environment -> String  
get_param1 (Environment p1 _ _) = p1  

get_param2 :: Environment -> String  
get_param2 (Environment _ p2 _) = p2

get_param3 :: Environment -> String  
get_param3 (Environment _ _ p3) = p3

test_reader1 :: Reader Environment String
test_reader1 = do
    env <- ask        
    p3<-test_reader2
    return ("param1:"++ get_param1 env ++ " - " ++ p3)

test_reader2 :: Reader Environment String
test_reader2 = do
    env <- ask            
    return ("param3:"++get_param3 env)

loadEnv' ::  String -> Environment
loadEnv' s = Environment "Buddy" "Finklestein" "526-2928"

-- ========================================================================== --
-- main                                                                       --
-- ========================================================================== --

main :: IO ()
main = do
  -- let env = loadEnv' "/home/corrado/dev/vscode.wks/haskell.learning/HaskellLearning/src/config.txt"
  let env = loadEnv' ""
  let str = runReader test_reader1 env
  --let str = runReader func1 env
  print str


  {--



data Environment = Environment
  { param1 :: String
  , param2 :: String
  , param3 :: String }

stuff :: Reader Int String
stuff = do
  s <- ask
  return (show s ++ " green bottles")

env_cfg = Environment "Buddy" "Finklestein" "526-2928"
          
--main :: IO ()
--main = print $ runReader stuff 99


hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2


-- main = print . runReader convo $ "adit"

  --}