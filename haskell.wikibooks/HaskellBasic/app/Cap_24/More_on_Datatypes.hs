module Cap_24.More_on_Datatypes where

-- ========================================================================== --
-- Enumerations                                                               --
-- ========================================================================== --

-- One special case of the data declaration is the enumeration — a data type 
-- where none of the constructor functions have any arguments:
data Month = January | February | March | April | May | June | July
           | August | September | October | November | December

-- Main> :t January
-- January :: Month


-- You can mix constructors that do and do not have arguments, but then the 
-- result is not called an enumeration. The following example is not an 
-- enumeration because the last constructor takes three arguments:

data Colour = Black | Red | Green | Blue | Cyan
            | Yellow | Magenta | White | RGB Int Int Int

-- Main> :t White
-- White :: Colour

-- Main> :t RGB
-- RGB :: Int -> Int -> Int -> Colour


-- ========================================================================== --
-- Named Fields (Record Syntax)                                               --
-- ========================================================================== -- 

-- Consider a datatype whose purpose is to hold configuration settings. 
-- Usually, when you extract members from this type, you really only care 
-- about one or two of the many settings.

-- Moreover, if many of the settings have the same type, you might often find
-- yourself wondering "wait, was this the fourth or fifth element?" 
-- One way to clarify is to write accessor functions. 
-- Consider the following made-up configuration type for a terminal program:

data Configuration = Configuration
    String  -- User name
    String  -- Local host
    String  -- Remote host
    Bool    -- Is guest?
    Bool    -- Is superuser?
    String  -- Current directory
    String  -- Home directory
    Integer -- Time connected
    deriving (Eq, Show)

-- You could then write accessor functions, such as:

getUserName   (Configuration un _ _ _ _ _ _ _) = un
getLocalHost  (Configuration _ lh _ _ _ _ _ _) = lh
getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh
getIsGuest    (Configuration _ _ _ ig _ _ _ _) = ig

-- And so on...

-- Main> getUserName (Configuration "John" "127.0.0.1" "127.0.0.1" False True 
--       "/home/john/apps" "/home/john" 12132)
-- "John"

-- You could also write update functions to update a single element. 
-- Of course, if you add or remove an element in the configuration later, all
-- of these functions now have to take a different number of arguments. 
-- This is quite annoying and is an easy place for bugs to slip in. 
-- Thankfully, there's a solution: we simply give names to the fields in the
-- datatype declaration, as follows:

data Configuration' = Configuration'{ 
    username        :: String , 
    localHost       :: String,
    remoteHost      :: String,
    isGuest         :: Bool, 
    isSuperuser     :: Bool,
    currentDir      :: String,
    homeDir         :: String,
    timeConnected   :: Integer    
}

-- This will automatically generate the following accessor functions for us:

--username    :: Configuration' -> String
--localHost   :: Configuration' -> String