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
    , otherwise
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
    show O      = "O"
    show (S n)  = "S" ++ show n

instance Eq Nat where
    O == O         = True
    (S n) == (S m) = n == m
    _ == _         = False

instance Ord Nat where
    O <= _         = True
    (S n) <= O     = False
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O n         = O
    min n O         = O
    min (S n) (S m) = S (min n m)

    max O n         =  n
    max n O         =  n
    max (S n) (S m) = S (max n m)


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

isZero :: Nat -> Bool
isZero O     = True
isZero (S n) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n

even :: Nat -> Bool
even O     = True
even (S n) = odd n

odd :: Nat -> Bool
odd n = not (even n)

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O         = n
monus O (S n)     = O
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

infixl 6 -*

-- multiplication
times :: Nat -> Nat -> Nat
times _ O     = O
times n (S m) = (times n m) + n

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O     = S O
pow n (S m) = (pow n m) <*> n

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
n </> m
  | not (m <= n) = O
  | otherwise    = S ((n -* m) </> m)

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = undefined
n <%> m = n -* ((n / m) * m)

infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = ((n </> m), (n <%> m))

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> _ = undefined
m <|> n
  | n <%> m == O = True
  | otherwise    = False

divides = (<|>)

infixl 5 <|>


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m
  | n -* m == O = m -* n
  | otherwise   = n -* m

(|-|) = dist

infixl 6 |-|

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = factorial n * (S n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _     = undefined
lo (S O) _ = undefined
lo n m
  | not (n <= m) = O
  | otherwise    = S (lo n (m / n))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat x
  | not (0 <= x) = undefined
  | x == 0       = O
  | otherwise    = S (toNat (a - 1))

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

