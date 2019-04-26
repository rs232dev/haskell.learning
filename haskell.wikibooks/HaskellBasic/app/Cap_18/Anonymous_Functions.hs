module Cap_18.Anonymous_Functions where

-- ========================================================================== --
-- Anonymous Functions - lambdas                                              --
-- ========================================================================== --    

-- sumStr''= foldl addStr 0.0
--    where addStr x str = x + read str

-- Why create a formal name for a function like addStr when it only exists 
-- within another function's definition, never to be used again? Instead, we
-- can make it an anonymous function also known as a ”lambda function”. 
-- Then, sumStr could be defined like this:

-- Main> :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- -------------------------------------------------------------------------- --
-- The expression in the parentheses is a lambda function                     --
-- -------------------------------------------------------------------------- --
lsumStr:: [String] -> Float
lsumStr = foldl (\ x str -> x + read str) 0.0

-- The backslash is used as the nearest ASCII equivalent to the Greek letter 
-- lambda (λ). This lambda function takes two arguments, x and str, and it 
-- evaluates to "x + read str". So, the sumStr presented just above is precisely
-- the same as the one that used addStr in a let binding.

-- Lambdas are handy for writing one-off functions to be used with maps, folds
-- and their siblings, especially where the function in question is simple 
-- (beware of cramming complicated expressions in a lambda — it can hurt 
-- readability).

-- Since variables are being bound in a lambda expression (to the arguments, 
-- just like in a regular function definition), pattern matching can be used
-- in them as well. A trivial example would be redefining tail with a lambda:

tail':: [a] -> [a]
tail' = (\ (_:xs) -> xs)

-- Main> tail' [1,2,3,4]
-- [2,3,4]

-- Note: Since lambdas are a special character in Haskell, the \ on its own
-- will be treated as the function and whatever non-space character is next
-- will be the variable for the first argument. It is still good form to put
-- a space between the lambda and the argument as in normal function syntax 
-- (especially to make things clearer when a lambda takes more than one 
-- argument).


