module Cap_29.Applicatives_tutorial where


-- ========================================================================== --
-- Applicatives                                                               --
-- ========================================================================== --

-- The Functor typeclass llows us to run transformations on data regardless of 
-- how the data is wrapped. No matter if our data were in a List, a Maybe, an 
-- Either, or even a custom type, we could simply call fmap. 
-- However, what happens when we try to combine wrapped data? 
-- For instance, if we try to have GHCI interpret these calculations, we’ll get
-- type errors:

-- (Just 4) * (Just 5)
-- Nothing * (Just 2)

-- Can functors help us here? We can use fmap to wrap multiplication by the 
-- particular wrapped Maybe value:

funct :: Maybe (Integer -> Integer)
funct = (*) <$> (Just 4)

-- the above function is the same of:

funct' :: Maybe (Integer -> Integer)
funct' = fmap (*) (Just 4)

-- This gives us a partial function wrapped in a Maybe. 
-- But we still cannot unwrap this and apply it to (Just 5) in a generic fashion. 
-- So we have to resort to code specific to the Maybe type: 

funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe Nothing _ = Nothing
funcMaybe (Just f) val = fmap f val

-- Main> funcMaybe (Just (+1)) (Just 2)
-- Just 3


-- Applicatives to the Rescue
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- The pure function takes some value and wraps it in a minimal context. 

-- The <*> function, called sequential application, takes two parameters:
--
--  * First, it takes a function wrapped in the context.
--  * Next, a wrapped value. 
--
-- Its output is the result of applying the function to the value, rewrapped in
-- the context. 
-- 
-- An instance is called an applicative functor because it allows us to apply 
-- a wrapped function. 

-- Since sequential application takes a wrapped function, we typically begin 
-- our use of applicatives by wrapping something with either pure or fmap. 
-- This will become more clear with some examples.

app1 = pure (4 *) <*> (Just 5)
--
-- Main> app1
-- Just 20

app2 = pure (4 *) <*> Nothing
--
-- Main> app2
-- Nothing


-- Now if we want to multiply 2 maybe values, we start by wrapping the bare 
-- multiplication function in pure. Then we sequentially apply both Maybe 
-- values:

app3 = pure (*) <*> (Just 4) <*> (Just 5)


app4 :: Maybe (Integer -> Integer)
app4  = (fmap (*) (Just 4))

app4' :: Maybe Integer
app4' = (<*>) app4 (Just 2)
-- 
-- *Main> app4'
-- Just 8

-- -- (<$>) is merely an infix synonym for fmap, so you can write e.g.
app5 :: Maybe (Integer -> Integer)
app5  = (*) <$> (Just 4)



