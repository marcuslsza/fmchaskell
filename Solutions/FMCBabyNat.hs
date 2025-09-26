module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + (S m) = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S n) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O 
even (S O) = O
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O n = O
monus n O = n
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = n * m + n

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ S O = n
n ^ (S m) = n ^ m * n

infixl 8 ^

-- defining less than 
(<) :: Nat -> Nat -> Nat
n < O = O
O < n = S O
(S n) < (S m) = n < m

-- quotient
(/) :: Nat -> Nat -> Nat
n / O = undefined
O / n = O
n / m  =  case n < m of
            S O -> O
            O -> S((n -* m)/m)

-- remainder
(%) :: Nat -> Nat -> Nat
n % m = n-*(m*(n/m))

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
n ||| m = case O < (n%m) of
            S O -> O
            O -> S O

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O m = m
absDiff m O = m 
absDiff (S n) (S m) = absDiff n m


(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S O) = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O a = undefined
lo b O = undefined
lo b (S O) = O
lo (S O) a = undefined
lo b a = S (lo b (a/b))

