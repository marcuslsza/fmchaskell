module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True 

instance Show Bool where

    show True = "True"
    show False  = "False"

instance Enum Bool where
    
    toEnum 1 = True
    toEnum 0 = False

    fromEnum False = 0
    fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
False && _ = False
_ && False = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
True || _ = True
_ || True = True
False || False = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
False /|\ _ = True
_ /|\ False = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = False
True \|/ _ = False
False \|/ _ = False


infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True <=/=> False = True
False <=/=> True = True
True <=/=> _ = False
False <=/=> _ = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not False = True
not True = False

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a b = a

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> True = True
False ==> False = True
True ==> False = False
False ==> True = False

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
True <== True = True
False <== False = True
True <== False = False
False <== True = False

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = False
True <=> False = False
False <=> True = True

infixr 1 <=>


