module Utils.Start where 


{- =============================================================================
   ==                       function definition                               ==
   =============================================================================
-}
square x = x*x


-- function definition (local variable definition by where)
-- The variable s is half the perimeter of the triangle and it would be tedious
-- to write it out four times in the argument of the square root function sqrt.
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
            where
                s = (a+b+c) / 2

f1 x = x + 3
test2 = f1 2 == 5

{- =============================================================================
   ==                         Prefix / Infix Operator                         ==
   =============================================================================
Haskell allows two-argument functions with names composed only of
non-alphanumeric characters to be used as infix operators, that is, placed
between their arguments. The only caveat is that if you wish to use such a
function in the "standard" way (writing the function name before
the arguments, as a prefix operator) the function name must be enclosed in
parentheses. So the following expressions are completely equivalent:
-}
func_with_prefix_operator x y = (+) x y
func_with_infix_operator x y  =  x + y


{- =============================================================================
   ==                                 Guards                                  ==
   =============================================================================
To show how guards work, we are going to implement the absolute value function.
The absolute value of a number is the number with its sign discarded3; so if the
number is negative (that is, smaller than zero) the sign is inverted; otherwise
it remains unchanged.

The key feature of the definition is that the actual expression to be used for
calculating |x| depends on a set of propositions made about x. If x  0 we use
the first expression, but
if x < 0 we use the second one instead. If we are going to implement the
absolute value function in Haskell we need a way to express this decision
process. That is exactly what guards help us to do.
-}
fn_abs x
  | x < 0 = 0 - x
  | otherwise = x



{- =============================================================================
   ==                         List and Tuple                                  ==
   =============================================================================
-}
numbers = [1,2,3,4]


{-
     Building lists

In addition to specifying the whole list at once using square brackets and
commas, you can build them up piece by piece using the (:) operator.
This process is often referred to as
consing1.
-}
numbers_2 = 10:11:numbers

-- Lists within lists
listOfLists = [[1,2],[3,4],[5,6]]


-- Tuples
pair = (5,"five")
mytuple = (4, 5, "Six", True, 'b')
get4th (_,_,_,a,_) = a

{- =============================================================================
   ==                         polymorphic                                     ==
   =============================================================================
-}

plength ::  [a] -> Int
plength [] = 0
plength (x:xs) = length (x:xs)

ll1 = [1,2,3,4,5]
ll2 = ['a','b','c','d']
ll3 = ["hello","world"]

myfoldr:: (a -> b -> b) -> b -> [a] -> b
myfoldr f z  []    = z
myfoldr f z  (x:xs) =  f x (myfoldr f z xs)

-- definition of type Tree
-- Tree is a type constructor
-- Empty | Node is a data constructor
data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show) -- binary tree

-- ========================================================================= ==
-- To say that some type constructor actually is a functor, you say that it's an
-- instance of a class.
-- So "class Functor f ..." is called Type class
-- ========================================================================= ==
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- build a tree variable of type Tree
tree  =  Node (Empty) 7 (Empty)

-- fmap on tree
treedata = fmap (\x -> x+1) tree

-- ========================================================================= ==
--  recursion by helper function
-- ========================================================================= ==
mySum xs = helper 0 xs
  where helper acc (x:xs) = helper (acc + x) xs
        helper acc _ = acc

-- ========================================================================= ==
--  function composition by . operator
-- ========================================================================= ==
fun1 = \x -> x * x
fun2 = \x -> x + 1
comp2 xs = map (fun1 . fun2) xs


