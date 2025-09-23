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
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O     = S O
isZero (S n) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O     = S O
even (S n) = odd n

odd :: Nat -> Nat
odd O      = O
odd (S n)  = even n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns O
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O         = n
monus O (S n)     = O
monus (S m) (S n) = monus m n

(-*) :: Nat -> Nat -> Nat
(-*) = monus

infixl 6 -*

-- multiplication
(*) :: Nat -> Nat -> Nat
m * O     = O
m * (S n) = (m * n) + m

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
m ^ O     = one
m ^ (S n) = (m ^ n) * m

infixr 8 ^

-- less than or equal
(<=) :: Nat -> Nat -> Nat
O <= _ = S O
S _ <= O = O
(S n) <= (S m) = n <= m

infix 5 <=

-- quotient
(/) :: Nat -> Nat -> Nat
m / O = undefined
m / n =
  case n <= m of 
    O -> O
    S O -> S ((m -* n) / n)

infixl 8 /

-- remainder
(%) :: Nat -> Nat -> Nat
m % 0 = undefined
m % n = m -* ((m / n) * n)

infixl 8 %

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O ||| n = undefined
m ||| n = 
  case n % m of
    O -> S O
    _ -> O

infixl 9 |||

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff m n =
  case m -* n of
    O -> n -* m
    _ -> m -* n

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

infixl 6 |-|

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = factorial n * (S n)

-- signum of a number (-1, O, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O m     = undefined
lo (S O) m = undefined
lo n m     =
  case m / n of
    O -> O
    _ -> S (lo n (m / n))
