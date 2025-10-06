{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

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

length :: [a] -> Int
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
minimum [] = error "Lista vazia"
minimum [a] = a
minimum (x:xs) = if x < minimum xs then x else minimum xs

maximum :: Ord a => [a] -> a
maximum [] = error "Lista vazia"
maximum [a] = a
maximum (x:xs) = if x > maximum xs then x else maximum xs

take :: Int -> [a] -> [a]
take 0 _ = [] 
take n [] = error "Lista vazia"
take n (x:xs) = x:take (n-1) xs

drop :: Int -> [a] -> [a]
-- drop 2 [1,2,3,4] = drop 1 [2,3,4] = drop 0 [3,4] = [3,4]
drop _ [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
--takeWhile (> 5) [6,8,7,2,1] = [6,8,7]
takeWhile _ [] = []  
takeWhile f (x:xs) = if f x then x:takeWhile f xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f (x:xs) = if f x then dropWhile f xs else x:xs


tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = xs:tails xs 


init :: [a] -> [a]
init [] = error "Lista vazia"
init [a] = []
init (x:xs) = x:init xs 

inits :: [a] -> [[a]]
inits [] = [[]]
-- usa map pra aplicar a função x: em inits xs 
inits (x:xs) = []:map (x:) (inits xs)

-- subsequences (?)

any :: (a -> Bool) -> [a] -> Bool
-- any (> 3) [1,2,3,4] = any (> 3) [2,3,4] = any (> 3) [3,4] = any (> 3) [4] => x := 4 && 4 > 3 => True
any f [] = False
any f (x:xs) = if f x then True else any f xs

all :: (a -> Bool) -> [a] -> Bool
-- all (> 3) [1,2,3,4] 
all f [] = True
all f (x:xs) = if f x then all f xs else False


and :: [Bool] -> Bool 
-- and [True, True] = and [True] = and [] = True
and [] = True
and (x:xs) = if x == True then and xs else False

or :: [Bool] -> Bool
-- or [False, False] = or [False] = False
or [False] = False
or (x:xs) = if x == True then True else or xs 


-- concat

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs) = any (== n) (x:xs)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False 
elem' n (x:xs) = if n == x then True else elem' n xs


(!!) :: [a] -> Int -> a
-- res 3 <= (!!) 2 [1,2,3,4] = 
(!!) [] _ = error "Lista vazia"
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)


-- filter (analyzes bool condition; "a < 3")
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = if f x then x:filter f xs else filter f xs


map :: (a -> b) -> [a] -> [b]
-- map (*2) [1,2,3,4] returns [2,4,6,8]
map f [] = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "Lista vazia"
cycle (x:xs) = x:xs ++ cycle (x:xs)

repeat :: a -> [a]
repeat n = n:repeat n

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a:replicate (n-1) a

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] xs = True
isPrefixOf xs [] = False
isPrefixOf (x:xs) (y:ys) = if x == y then isPrefixOf xs ys else False

-- isInfixOf

-- isSuffixOf

zip :: [a] -> [a] -> [a]
zip [] xs = xs
zip xs [] = xs
zip (x:xs) (y:ys) = x:y:zip xs ys


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ xs [] = []
zipWith _ [] xs = []
zipWith f (x:xs) (y:ys) = f x y:zipWith f xs ys

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [a] = a
intercalate x (y:ys) = y ++ x ++ intercalate x ys

nub :: Eq a => [a] -> [a]
--nub [1,2,2,3,4]
nub = undefined


splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs = ([], xs)
splitAt _ [] = ([], [])
-- usando "let in" para decompor a tupla:
splitAt n (x:xs) = if n > 0 then let (ys, zs) = splitAt (n-1) xs in (x:ys, zs) else ([], x:xs)

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- resposta: não há uma condição para o caso de n > length xs, resultando em um erro de callstack. 

break :: (a -> Bool) -> [a] -> ([a], [a])
-- break (== 3) [1,2,3,4] = [1], break (== 3) [2,3,4] = [1,2], break (== 3) [3,4] = ([1,2], [3,4]).
break _ [] = ([], [])    
break f (x:xs) = if f x then ([], x:xs) else let (ys, zs) = break f xs in (x:ys, zs)

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

