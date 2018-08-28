module ReaderMonad where

import Control.Monad.ST
import Control.Monad.Reader
--import Control.Monad.IO.Class
    

-- Imagine this is a directory
type Config = FilePath

{--
load :: Config -> String -> IO String
load config x = readFile (config)

loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll config x y = do
    a <- load config y
    b <- loadRevision config x
    return (a, b)
--}

-- λ:loadRevision "/home/corrado/dev/vscode.wks/haskell.learning/HaskellLearning/src/config.txt" 1
-- "\n\nparams_1 = one\nparams_2 = two\nparams_3 = three\n\n"


stuff :: Reader Int String
stuff = do
  s <- ask
  return (show s ++ " green bottles")
    
main :: IO ()
main = print $ runReader stuff 99