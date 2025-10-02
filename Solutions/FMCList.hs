{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import FMCNat

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null xs = False

length :: [a] -> Nat
length [] = 0
length (_:xs) = length xs + 1


sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = sum xs + x

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = product xs * x

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x:xs) ++ ys = x:(xs++ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++


-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (a:as) = (a:as) ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [a] = a
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [a] = a
maximum (x:xs) = max x (maximum xs)

take :: a -> [a] -> [a]
take n [] = []
take n (x:xs) = undefined



-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above 

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter (analyzes bool condition; "a < 3")
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = error "empty list"
filter f (x:xs) = if f x then x:filter f xs else filter (f xs)


map :: (a -> b) -> ([a] -> [b])
map f [] = error "empty list"
map f (x:xs) = (f x):map f xs

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

