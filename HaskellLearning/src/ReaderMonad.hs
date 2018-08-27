module ReaderMonad where

import Control.Monad.ST

-- Imagine this is a directory
type Config = FilePath

load :: Config -> String -> IO String
load config x = readFile (config)

loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll config x y = do
    a <- load config y
    b <- loadRevision config x
    return (a, b)


data AppConfig = AppConfig {
    logfile :: FilePath
    , version :: String
    , maxMessageLength :: Int
} deriving (Show, Read)

newtype CReader a = CReader { runCR :: AppConfig -> a }

instance Applicative (CReader a) where
    pure = return
    (<*>) = ap    

instance Functor CReader where
    fmap f cr = CReader $ \c -> f (runCR cr c)

instance Monad CReader where
    -- return :: a -> CReader a
    return = CReader . const
    -- >>= :: CReader a -> (a -> CReader b) -> CReader b
    a >>= f = CReader $ \c -> runCR (f ((runCR a) c)) c

-- λ:loadRevision "/home/corrado/dev/vscode.wks/haskell.learning/HaskellLearning/src/config.txt" 1
-- "\n\nparams_1 = one\nparams_2 = two\nparams_3 = three\n\n"

{--
load' :: (MonadReader Config m, IO m) => String -> m String
load' x = do
    config <- ask
    liftIO $ readFile (config ++ x)

loadRevision' :: (MonadReader Config m, IO m) => Int -> m String
loadRevision' x = load ("history" ++ show x ++ ".txt")

loadAll' :: (MonadReader Config m, IO m) => Int -> String -> m (String, String)
loadAll' x y = do
    a <- load' y
    b <- loadRevision' x
    return (a, b)

--}