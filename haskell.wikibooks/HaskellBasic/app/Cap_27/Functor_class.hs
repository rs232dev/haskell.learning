module Cap_27.Functor_class where


-- ========================================================================== --
-- Functor Class                                                              --
-- ========================================================================== --    

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- example of Functor instance
-- instance Functor Tree where
--    fmap f (Leaf x) = Leaf (f x)
--    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- example of fmap (List) 
fList = fmap (2*) [1,2,3,4]

-- Main> fList
-- [2,4,6,8]

-- example of fmap (Maybe) 
fMaybe = fmap (2*) (Just 1)

-- Main> fMaybe 
-- Just 2




