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
-- Example of Environment by argument passed through                         --
-- ========================================================================== --

func_environment_1 :: Environment -> String
func_environment_1 env = "Result: " ++ (show (func_environment_2 env))

func_environment_2 :: Environment -> String
func_environment_2 env = (show (func_environment_3 env))

func_environment_3 :: Environment -> Int
func_environment_3 env = 2


-- ========================================================================== --
-- Example of Monad Reader (loadEnv' from Environment Record type)            --
-- ========================================================================== --

data Environment = Connection{   
    uri :: String
  , usr :: String
  , pwd :: String 
} deriving (Show)  

get_uri :: Environment -> String  
get_uri (Connection p1 _ _) = p1  

get_usr :: Environment -> String  
get_usr (Connection _ p2 _) = p2

get_pwd :: Environment -> String  
get_pwd (Connection _ _ p3) = p3

func_reader_1 :: Reader Environment String
func_reader_1 = do
    env <- ask        
    p2<-func_reader_2
    return ("uri:"++ get_uri env ++ " - " ++ p2)

func_reader_2 :: Reader Environment String
func_reader_2 = do
    env <- ask       
    p3<-func_reader_3     
    return ("usr:"++get_usr env ++ " - "++p3)

func_reader_3 :: Reader Environment String
func_reader_3 = do
    env <- ask            
    return ("pwd:"++get_pwd env)    

loadEnv' ::  String -> Environment
loadEnv' s = Connection "Buddy" "Finklestein" "526-2928"

-- convo = hello >>= \h -> (bye >>= \b -> return $ h ++ b)
mrbind = func_reader_1 >>= \x -> (func_reader_2 >>= \y -> return $ "("++ x ++ " -- " ++ y++")")

-- ========================================================================== --
-- main                                                                       --
-- ========================================================================== --

main :: IO ()
main = do
  -- let env = loadEnv' "/home/corrado/dev/vscode.wks/haskell.learning/HaskellLearning/src/config.txt"
  let env = loadEnv' ""
  let str = runReader func_reader_1 env
  let x = ask env
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