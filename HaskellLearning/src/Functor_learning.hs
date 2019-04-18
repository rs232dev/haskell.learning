module Functor_learning where 

    import Data.Functor
    import Data.Set
    import Data.Vector
    
    -- import Data.Functor.Contravariant.Compat
    
    
    
    -- function: f
    -- *Main> :t length [1,2,3]
    -- length [1,2,3] :: Int
    
    -- function: g (a is an Int)
    -- *Main> :t (>10)
    -- (>10) :: (Ord a, Num a) => a -> Bool
    
    
    -- (.)        f g x = f (g x)
    
    {--
    instance Functor  ((->)a) where
        fmap:: (x -> y) -> (a -> x) -> (a -> y)
        fmap = (.)
    
    --}
    
    
    
    
    newtype Op z a = Op (a -> z)
    
    cfmap :: (a -> b) -> (b ->c) -> (a ->c)
    cfmap f g x = g(f x)
    
    --contraMap h (Op g) = Op (g.h)
    
    lengther :: Op Int [a]
    lengther = Op Prelude.length
    
    lengther' ::[a] -> Int
    lengther' = Prelude.length
    
    lengther'' ::[a] -> String
    lengther'' = (show . Prelude.length)
    
    -- fmap      f g x = f(g x)
     --cfMap f g x = g(f x)
    
    cov'  = fmap lengther'  Data.Set.toList
    cov'' = fmap lengther'' Data.Set.toList
    
    
    
    --setLengther  :: Op Int (Set a)
    --setLengther = contraMap (Data.Set.toList) lengther
    myset    = Data.Set.fromList [1..20]
    -- mylist x = Data.Set.toList x
    mylist = Data.Set.toList
    
    
    fn = (cfmap Data.Set.toList lengther') 
    
    -- λ::t fn
    -- fn :: Set a -> Int
    -- λ:fn myset
    --20
    
    
    -- 
    -- λ:Data.Vector.fromList [1..20]
    -- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
    v1 = Data.Vector.fromList [1..25]
    
    -- λ::t v1
    -- v1 :: (Num a, Enum a) => Vector a
    
    fn1 = (cfmap Data.Vector.toList lengther') 
    -- λ::t fn1
    -- fn1 :: Vector a -> Int
    -- λ:fn1 v1
    -- 25
    {--
    class Contravariant f where
        cfmap :: (a -> b) -> f b -> f a
    
    newtype Predicate a = Predicate { getPredicate :: a -> Bool }
    
    instance Contravariant Predicate where
        cfmap g (Predicate p) = Predicate (p . g)
    --                          f           g        
    --                      
    -- veryOdd = contramap (`div` 2) (Predicate odd)
    -- print $ getPredicate veryOdd <$> [0 .. 11]
    -- [False,False,True,True,False,False,True,True,False,False,True,True]
    
    -- odd :: Integral a => a -> Bool
    -- (<$>) :: Functor f => (a->b) -> f a -> f b
    -- It's merely an infix synonym for fmap, so you can write e.g.
    -- Prelude> (*2) <$> [1..3]
    -- [2,4,6]
    -- Prelude> show <$> Just 11
    -- Just "11"
    
    
    u :: Set a -> [a]
    u set = Data.Set.toList set
    
    z :: Vector a -> [a]
    z vect = Data.Vector.toList vect
    --}
    
    
    
    
    j :: (Set a -> [a])
    j = Data.Set.toList 
    
    
    
    safe_fun :: Num p => (t -> p) -> Maybe t -> p
    safe_fun f x = case x of 
    {
        Nothing -> 0; 
        Just i -> f i 
    } 
    
    m1 = Just(2)
    m2 = Nothing
    
    
    
    safe_val x = case x of 
    {
        Nothing -> 0; 
        Just i ->  i 
    } 
    
    safe_compose :: Num b => (b -> c) -> Maybe b -> c
    safe_compose f = f . safe_val
    
    
    