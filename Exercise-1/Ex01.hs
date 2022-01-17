module Ex01 where
import Data.Char (toUpper)

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


{- Part 1

Write a function 'raise' that converts a string to uppercase

Hint: 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged.
It is imported should you want to use it.

-}
raise :: String -> String
raise [] = []                             --If we are given an empty string, we can't convert anything to uppercase.
raise (x:xs) = toUpper x : raise xs       --If the string is non-empty, we simply convert the first element (i.e. character) of the list (i.e. String) and do this recursively.

{- Part 2

Write a function 'nth' that returns the nth element of a list.
Hint: the test will answer your Qs

-}
nth :: Int -> [a] -> a
nth 1 (x:xs) = x                         --The first element of the list x:xs is just x
nth z (x:xs) = nth (z-1) xs              --Any other element can be obtained using recursion, simply decreasing the paramater z.

{- Part 3

Write a function `commonLen` that compares two sequences
and reports the length of the prefix they have in common.

-}
commonLen :: Eq a => [a] -> [a] -> Int
commonLen [] _ = 0                        -- Nothing in common here!
commonLen _ [] = 0                        -- Nothing in common here too!
commonLen (x:xs) (y:ys) =
       if x==y
        then ((commonLen xs ys)+1)        -- If both have the same element, recursively check if the next one is the same too, add 1 as we go along!
        else 0

{- Part 4

(TRICKY!) (VERY!)

Write a function `runs` that converts a list of things
into a list of sublists, each containing elements of the same value,
which when concatenated together give the same list

So `runs [1,2,2,1,3,3,3,2,2,1,1,4]`
 becomes `[[1],[2,2],[1],[3,3,3],[2,2],[1,1],[4]]`

Hint:  `elem :: Eq a => a -> [a] -> Bool`

HINT: Don't worry about code efficiency
       Seriously, don't!

-}
runs :: Eq a => [a] -> [[a]]
runs [] = []                                                   -- An empty list has no sublist
runs(x:[]) = [[x]]                                             -- A singleton list has only one option for the sublist
runs (x:xs) = a:(runs b) where (a,b) = span (==x) (x:xs)       -- Use span and recursion to group the elements without using group function in prelude. 