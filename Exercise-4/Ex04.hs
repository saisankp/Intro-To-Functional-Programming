{- butrfeld Andrew Butterfield -}
module Ex04 where

name, idno, username :: String
name      =  "Prathamesh Sai"  -- replace with your name
idno      =  "19314123"    -- replace with your student id
username  =  "saisankp"   -- replace with your TCD username

declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
--ins k d Empty = Leaf k d
ins k d Empty = Leaf k d
ins k d (Leaf a b) 
    | k < a = Branch (Leaf k d) (Empty) a b
    | k > a = Branch (Empty) (Leaf k d) a b
    | otherwise = Leaf k d
ins k d (Branch left right a b)
    | k < a = Branch (ins k d left) right a b
    | k > a = Branch left (ins k d right) a b
    | otherwise = Branch left right k d

    

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp Empty _ = fail "Nothing"
lkp (Leaf a b) k
    | k < a = fail "Nothing"
    | k > a = fail "Nothing"
    | otherwise = return b
lkp (Branch left right a b) k
    | k < a = lkp left k
    | k > a = lkp right k
    | otherwise = return b

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds = getLASs init1 init2 init3 ds
init1 = 0
init2 = 0
init3 = 0

{-
  Implement the following tail-recursive  helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs a b c d = (lengthOfList d+a, sumOfList d+b, sumOfList squaredList+c) where squaredList=sumOfSquaredList d

-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)

-- Helper function for the helper function! Uses tail recursion.
lengthOfList :: [a] -> Int
lengthOfList = lengthOfList' 0
    where lengthOfList' a [] = a
          lengthOfList' a (_:xs) = lengthOfList' (a+1) xs

-- Helper function for the helper function! Uses tail recursion.
sumOfList:: Num a => [a] -> a
sumOfList x = iterate x 0
    where
    iterate x y | null x    = 0 + y
                | otherwise = iterate (tail x) $ head x + y

-- Helper function for the helper function! Uses a function that uses tail recursion.
sumOfSquaredList :: [Double] -> [Double]
sumOfSquaredList x = tailRecursiveMap (^2) x

-- Helper function for the helper function for the helper function! Uses tail recursion.
tailRecursiveMap :: (a -> b) -> [a] -> [b]
tailRecursiveMap func = helper [] where
  helper acc [] = reverse acc
  helper acc (h:t) = helper (func h : acc) t
