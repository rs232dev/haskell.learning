module GH_Course4 where


import Data.Monoid

-- ========================================================================== ==    
--     __  _______  _  ______  _______  ____                                  ==
--    /  |/  / __ \/ |/ / __ \/  _/ _ \/ __/                                  ==
--   / /|_/ / /_/ /    / /_/ // // // /\ \                                    ==
--  /_/  /_/\____/_/|_/\____/___/____/___/                                    ==
-- ========================================================================== ==    
{--

class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

  -- Defined in ‘GHC.Base’
  instance Monoid [a]                      -- Defined in ‘GHC.Base’
  instance Monoid Ordering                 -- Defined in ‘GHC.Base’
  instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
  instance Monoid a => Monoid (IO a)       -- Defined in ‘GHC.Base’
  instance Monoid b => Monoid (a -> b)     -- Defined in ‘GHC.Base’
  instance (Monoid a, Monoid b, Monoid c, 
        Monoid d, Monoid e) 
                 => Monoid (a, b, c, d, e)      -- Defined in ‘GHC.Base’
  
  instance (Monoid a, Monoid b, Monoid c,
       Monoid d) => Monoid (a, b, c, d)         -- Defined in ‘GHC.Base’
  
  instance (Monoid a, Monoid b, Monoid c) 
                       => Monoid (a, b, c)      -- Defined in ‘GHC.Base’
  
instance (Monoid a, Monoid b) => Monoid (a, b)  -- Defined in ‘GHC.Base’
instance Monoid ()                              -- Defined in ‘GHC.Base’

--}

{-- 
instance Monoid [a] where

 -- mempty :: [a]
    mempty = []

 -- mappend :: [a] ->[a] -> [a]
    mappend = (++)
--}

-- Monoid laws:

-- λ:mempty `mappend` x        = x                             -- left identity
-- x `mappend` mempty          = x                             -- right identity
-- x `mappend` (y `mappend` z) =   (x `mappend` y) `mappend` z -- associativity 

-- Example of Left identity:
-- λ:mempty `mappend` [1,2,3]
-- [1,2,3]

-- Example of Right identity:
-- λ:[1,2,3] `mappend` mempty
-- [1,2,3]


-- Example of Associativity:
-- λ:[1,2,3] `mappend` ([4,5] `mappend` [6,7])
-- [1,2,3,4,5,6,7]

-- λ:([1,2,3] `mappend` [4,5]) `mappend` [6,7]
-- [1,2,3,4,5,6,7]
 
{--
instance (Monoid a) => Monoid (Maybe a) where 

 -- mempty :: Maybe a  
    mempty = Nothing
    
 -- mappend Maybe a -> Maybe a -> Maybe a 
    Nothing `mappend` mx  = mx
    mx  `mappend` Nothing = mx
    Just x `mappend` Just y = Just (x `mappend` y)

 
instance Monoid (Int) where 

    -- mempty :: Int
       mempty = 0
       
    -- mappend Int -> Int -> Int       
       mappend = (+)

--}
newtype Sum a = Sum a deriving(Eq, Ord,Show,Read)

