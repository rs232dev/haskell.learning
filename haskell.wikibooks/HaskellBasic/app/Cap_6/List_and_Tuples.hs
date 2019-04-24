module Cap_6.List_and_Tuples where

-- ========================================================================== --
-- List                                                                       --
-- ========================================================================== --
mylist = [2,3,4,5]
-- Main> mylist 
-- [2,3,4,5]


-- ========================================================================== --
-- Building a List                                                            --
-- ========================================================================== --
{-- In addition to specifying the whole list once using square brackets
    and commas, you can build them up piece by using the (:) operator.
    This process is often referred to as consing.
    "cons" happens to be a mnemonic fo "constructor".
--}

mylist2 = 1:[2,3,4,5]
-- Main> mylist2 
-- [1,2,3,4,5]

mylist3 = 0:1:[2,3,4,5]
-- Main> mylist3
-- [0,1,2,3,4,5]

empty_list = []

one_element_list  = [1]
one_element_list' = 1:[]
-- Main> empty_list 
-- []

-- Main> one_element_list
-- [1]

-- Main> one_element_list'
-- [1]

-- ========================================================================== --
-- List of Lists                                                              --
-- ========================================================================== --
lol = [[1,2,3], [4,5,6], [7,8,9]]
   
-- ========================================================================== --
-- Tuples                                                                     --
-- ========================================================================== --
{--
    Tuples are another way of storing values in a single value.
    There are two key differences between Tuples and List:
       
    - they have a fixed number of elements
    - the elements of a tuple do not need to be all of the same type                   
--}
my_tuple:: Num t => (t, [Char])
my_tuple = (1,"one")

-- ========================================================================== --
-- Retrieve values                                                            --
-- ========================================================================== --

fst_element = fst my_tuple
snd_element = snd my_tuple

-- Main> my_tuple 
-- (1,"one")
--
-- Main> fst my_tuple 
-- 1

-- Main> snd my_tuple 
-- "one"

-- ========================================================================== --
-- Tuples within tuples                                               --
-- ========================================================================== --
my_tuple':: Num t => (t, (t,[Char]))
my_tuple' = (1,(1,"one"))

-- Main> my_tuple'
-- (1,(1,"one"))
    
-- ========================================================================== --
-- Polymorphic types                                                          --
-- ========================================================================== --


-- from Prelude:
-- Main> :t length
-- length :: Foldable t => t a -> Int

-- Main> :t fst
-- fst :: (a, b) -> a

-- Main> :t snd
-- snd :: (a, b) -> b

{-- 
    The a or b in the above declarations are not a type.
    Remember that type names in Haskell start with uppercase letter.
    instead it's a TYPE VARIABLE.

    When Haskell sees a type variable, it allow ANY TYPE to take its place.
 
--}

int_list  = [2,3,4,5]
char_list = ['a','b','c']

-- Main> length int_list 
-- 4
-- Main> length char_list 
-- 3
    












