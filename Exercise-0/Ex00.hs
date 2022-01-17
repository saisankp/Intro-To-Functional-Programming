module Ex00 where

name, idno, username :: String
name      =  "Prathamesh Sai Sankar"  -- replace with your name
idno      =  "19314123"    -- replace with your student id
username  =  "saisankp"   -- replace with your TCD username


declaration -- do NOT modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

{- Modify everything below here to ensure all tests pass -}

hello  =  "Hello World :-)"
