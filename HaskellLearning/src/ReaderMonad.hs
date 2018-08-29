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

-- *ReaderMonad> runReader mrbind (loadEnv' "")
-- "(uri:Buddy - usr:Finklestein - pwd:526-2928 -- usr:Finklestein - pwd:526-2928)"

-- ========================================================================== --
-- Monad Reader studying                                                      --
-- ========================================================================== --
data Config = Config{
    user        :: String
    , password  :: String 
    , token     :: Int
} deriving (Show)  

users  =  [ Config "John"    "secret.john"    12345, 
            Config "Charlie" "secret.charlie" 54321,
            Config "Oliver"  "secret.oliver"  32123
          ]

find_user :: String -> [Config] -> Maybe Config
find_user _ [] = Nothing
find_user usr (x:xs)
  |  usr == (get_user x) = Just x
  | otherwise = find_user usr xs


get_user :: Config -> String  
get_user (Config p1 _ _) = p1  

get_password :: Config -> String  
get_password (Config _ p2 _) = p2

get_token :: Config -> Int  
get_token (Config _ _ p3) = p3


loadConfig ::  String -> Config
loadConfig s = Config "Buddy" "Finklestein" 12345


newtype Reader' e a = Reader' (e -> a)

runReader' :: Reader' e a  -> e  -> a
runReader'    (Reader' r) e =  r e

instance Functor (Reader' e) where
    fmap = liftM

instance Applicative (Reader' e) where
    pure = return
    (<*>) = ap    


instance Monad (Reader' e) where
    --  return    :: (Monad m) => a -> Reader a	
    return x  =  Reader' (\e -> x)

    --               __ m__  _a_         _ (a -> m b) _          _ m _  _ b_
    --              /      /    /       /              /       /      /    /
    --  (>>=)    :: Reader e  a     ->  (a -> Reader e b)    ->  Reader e  b
    m >>= k  = Reader'  (\e -> let a  = runReader' m e
                                   b  = k a
                                   in runReader' b e)

    -- reduced form of bind operator
    --    m >>= k  = Reader' ( \e -> runReader' (k ( runReader' m e )) e)   

get_config :: Maybe Config -> Config
get_config (Just x) = x
get_config Nothing  = Config "" "" 0
   
reader1 user = Reader' (\e ->  show $ get_token (get_config (find_user user e) ))
reader2 user = Reader' (\e ->  get_password (get_config (find_user  user e)))

elab  user = Reader' (\e ->  show $ get_token (get_config (find_user user e) )) >>= 
    \y -> Reader'(\e -> ("token:"++y++ " -- "++ (get_password (get_config (find_user  user e)))))

elab' user = reader1 user >>=  \y -> reader2 user


-- elab  = Reader' (\e ->  get_user e ) >>=  \y -> Reader'(\e -> get_password e)
-- *ReaderMonad> runReader' elab (loadConfig "")

-- λ:runReader' (elab "Oliver") users
-- "Oliver -- secret.oliver"



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