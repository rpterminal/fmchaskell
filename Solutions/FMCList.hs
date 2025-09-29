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
head []       = undefined
head (x : _) = x

tail :: [a] -> [a]
tail []       = undefined
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []       = 0
length (_ : xs) = 1 + (length xs)

sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product []       = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = (reverse xs) ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ xs       = xs
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []       = [x]
snoc y (x : xs) = x : (snoc y xs)

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
minimum []       = undefined
minimum [x]      = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum []       = undefined
maximum [x]      = x
maximum (x : xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x : xs)   = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (_ : xs)    = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x       = dropWhile p xs
  | otherwise = (x : xs)

tails :: [a] -> [[a]]
tails []       = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init []       = undefined
init [x]      = []
init (x : xs) = x : (init xs)

inits :: [a] -> [[a]]
inits []       = [[]]
inits (x : xs) = [] : [(x : p) | p <- inits xs]
-- list comprehension

subsequences :: [a] -> [[a]]
subsequences []       = [[]]
subsequences (x : xs) =
  let subs = subsequences xs
   in subs ++ [(x : s) | s <- subs]

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs)
  | p x       = True
  | otherwise = any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs)
  | p x       = all p xs
  | otherwise = False

and :: [Bool] -> Bool
and []       = True
and (b : bs) = b && (and bs)

or :: [Bool] -> Bool
or []       = False
or (b : bs) = b || (or bs)

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = x ++ (concat xs)

-- elem using the function 'any' above
elem :: Eq a => [a] -> Bool
elem e xs = any (== e) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' e []       = False
elem' e (x : xs)
  | e == x    = True
  | otherwise = elem' e xs

(!!) :: [a] -> Int -> a
[] !! _       = undefined
(x : xs) !! n
  | n < 0     = undefined
  | n == 0    = x
  | otherwise = xs !! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x       = x : (filter p xs)
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : (map f xs)

cycle :: [a] -> [a]
cycle [] = undefined
cycle xs = xs ++ (cycle xs)

repeat :: a -> [a]
repeat x = x : (repeat x)

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0    = []
  | otherwise = x : (replicate (n - 1) x)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _              = True
isPrefixOf (_ : _) []        = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _       = True
isInfixOf (_ : _) [] = False
isInfixOf xs ys      = isPrefixOf xs ys || isInfixOf xs (tail ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

zip :: [a] -> [b] -> [(a, b)]
zip [] _              = []
zip _ []              = []
zip (x : xs) (y : ys) = (x, y) : (zip xs ys)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _              = []
zipWith _ _ []              = []
zipWith f (x : xs) (y : ys) = (f x y) : (zipWith f xs ys)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ []       = []
intercalate _ [x]      = x
intercalate w (x : xs) = x ++ w ++ (intercalate w xs)

nub :: Eq a => [a] -> [a]
nub []       = []
nub (x : xs) = x : nub (filter (/= x) xs)

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
splitAt :: Int -> [a] -> ([a], [a])
splitAt _ []          = ([], [])
splitAt n xs | n <= 0 = ([], xs)
splitAt n (x : xs)    =
  let (ys, zs) = splitAt (n - 1) xs
   in (x : ys, zs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ []       = ([], [])
break p (x : xs)
  | p x       = ([], (x : xs))
  | otherwise =
      let (ys, zs) = break p xs
       in (x : ys, zs)

lines :: String -> [String]
lines "" = []
lines s = !TO-DO!

words :: String -> [String]
words "" = []
words s = !TO-DO!

unlines :: [String] -> String
unlines = !TO-DO!

unwords :: [String] -> String
unwords = !TO-DO!

transpose :: [[a]] -> [[a]]
transpose []       = []
transpose ([] : _) = []
transpose xs       = (map head xs) : transpose (map tail xs)

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s =
  let sFmt = map C.toLower (filter C.isAlpha s)
   in sFmt == reverse sFmt

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

