module Cap_18.More_on_Functions where


-- ========================================================================== --
-- let and where                                                              --
-- ========================================================================== --

addStr :: Float -> String -> Float
addStr x str = x + read str

sumStr,sumStr',sumStr'' :: [String] -> Float
sumStr = foldl addStr 0.0    

-- Main> :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--                   ________/            /      |      \
--                  /                    /       |       \ 
-- sumStr = foldl addStr 0.0____________/        |        \
--                 |                   /         |         \
--   (Float -> String -> Float)     Float     [String]     Float
--     b          a        b          b          t a         b                 

-- The above foldl function has been currified by: 
--
-- addStr : (b -> a -> b)
-- 0.0    : b                                                 

-- so the last input parameter 
-- [String] : t a  must be passed

-- and this represents the input parameters of the sumStr function.


-- we could rewrite sumStr using local bindings. We can do that either with
-- a let binding...

sumStr' =
    let addStr x str = x + read str
    in foldl addStr 0.0

-- ... or with a where clause...    

sumStr''= foldl addStr 0.0
    where addStr x str = x + read str

-- Main> sumStr ["1","2","4"]
-- 7.0

-- ... and the difference appears to be just a question of style.

-- read function: casts strings to another type 
-- Main> read "1"::Int
-- 1

