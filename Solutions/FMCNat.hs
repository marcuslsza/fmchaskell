{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise,
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n


instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    (S n) == O = False
    O == (S n) = False
    (S n) == (S m) = n == m

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= O = True
    n <= O = False
    O <= n = True
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min n m = if (n-*m) == (O-*n) then n else m

    max :: Nat -> Nat -> Nat
    max n m = if (n-*m) == (O-*n) then m else n


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

-- reciclando de FMCBabyNat.hs agora com o tipo bool. 

isZero :: Nat -> Bool
isZero O = True
isZero (S n) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> (S m) = S (n <+> m)

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
times :: Nat -> Nat -> Nat
times n O = O
times n (S O) = n
times n (S m) = times n m + n

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = pow n m <*> n

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> O = undefined
O </> n = O
n </> m  =  if n < m then O else S ((n -* m)</>m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = n-*(m<*>(n</>m))

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = if O < (n<%>m) then False else True

divides = (<|>)
-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n O = n 
dist O n = n
dist (S n) (S m) = dist n m

(|-|) = dist

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
lo b a = S (lo b (a</>b))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat x = if x < 0 then undefined else S (toNat(x-1))


fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = fromNat n + 1


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x-1))

