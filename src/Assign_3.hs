{- Assignment 3
 - Name: Sarib Kashif
 - Date: November 1 2019
 -}
module Assign_3 where
import qualified Data.Map.Strict as IM
 
macid :: String
macid = "kashis2"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyValue here
 - two simplest cases: 
 - 1. a constant (Coef x) will just return x
 - 2. an X will return n because x = n
 - if there is Sum/Prod of two expressions, evaluate them individually by 
 - calling polyValue for the two expressions seperately and adding/multiplying them
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue (Coef x) n = x
polyValue X n = n
polyValue (Sum x y) n = (polyValue x n) + (polyValue y n)
polyValue (Prod x y) n = (polyValue x n) * (polyValue y n)

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListValue here
 - simplest case is when the list is empty, which outputs 0
 - using horner's method, multiply the head with the n value for each
 - member of the list except the first
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue (PolyList (x:xs)) n = x + (n * polyListValue (PolyList xs) n)

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListSum here
 - abstract the list by calling an auxillary function which only takes the list component
 - add the heads of each list, then recurse without the heads
 - if either list is empty, return the non-empty list to place at the end of the final outputted list
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList s1) (PolyList s2) = PolyList (polyListSumAux s1 s2)

polyListSumAux :: (Num a,Eq a) => [a] -> [a] -> [a]
polyListSumAux [] s2 = s2
polyListSumAux s1 [] = s1
polyListSumAux (x:xs) (y:ys) = (x+y:polyListSumAux xs ys)
 
{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListDegree here
 - if empty list, then degree is 0
 - else, get the length of tail of the input (same as length of full list minus 1)
 - use 'toInteger' to convert 'Int' to 'Integer' 
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList []) = 0
polyListDegree (PolyList (x:xs)) = toInteger (length xs)

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListProd here
 - base case is when first list becomes empty, which is when the function ends.
 - any other case, multiply the head of list 1 with every member of list 2.
 - then recurse but only input the tail of list 1 and input list 2 with a 0 at start.
 - this is because the next head of list 1, when multiplied with all of list 2, will not have
 - a value in the first element so the 0 fills up the first element.
 - e.g [1,2,3] [4,5,6]
 - when multiplying 2 with list 2, you get [8,10,12] but 8 is 8x, so the 0 makes the list [0,8,10,12].
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) _ = (PolyList [])
polyListProd _ (PolyList []) = (PolyList [])
polyListProd (PolyList (x:xs)) (PolyList p2) = polyListSum (PolyList (map (x*) p2)) $polyListProd (PolyList xs) $PolyList (0:p2)

{- -----------------------------------------------------------------
- polyListToPoly
- -----------------------------------------------------------------
- Description: TODO add comments on polyListToPoly here
- simplest case is one element in list, which returns Coef of that element.
- if empty list is inputted, Coef 0 is returned, otherwise 0 is never retuned.
- last case uses horner's method: every time a new member of the list is added, it is multiplied by X as well 
- as the X's from previous recurses.
-}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList (x:[])) = Coef x
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) $Prod X $polyListToPoly $PolyList xs
  
{- -----------------------------------------------------------------
- polyToPolyList
- -----------------------------------------------------------------
- Description: TODO add comments on polyToPolyList here
- 1 case for every variation of Poly, similar to polyValue function
- 2 base cases:
- 1. Coef x returns a PolyList with x
- 2. X returns PolyList [0,1] because it is in the second index
- 2 recursive cases:
- 1. if two Polys are being added, apply polyListSum and recursively
  call the two Polys x and y seperately
- 2. if two Polys are multiplied, do the same as the first recursive case
  but apply polyListProd rather than polyListSum
-}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList (Coef x) = PolyList [x]
polyToPolyList X = PolyList [0,1]
polyToPolyList (Sum x y) = polyListSum (polyToPolyList x) (polyToPolyList y)
polyToPolyList (Prod x y) = polyListProd (polyToPolyList x) (polyToPolyList y)

{-
-----------------TEST CASES--------------
-----------------------------------------
Function: polyValue
Test Case Number: 1
Input: (Sum (Prod X (Coef (-2))) (X)) 4
Expected Output: -4
Actual Output: -4

Function: polyValue
Test Case Number: 2
Input: (Prod (X) (Sum (Prod (Coef 2) (X)) (X))) (-2)
Expected Output: 12
Actual Output: 12

Function: polyValue
Test Case Number: 3
Input: (Prod (Sum (Prod (Prod X X) X) X) (X)) 10
Expected Output: 10100
Actual Output: 10100

Function: polyListValue
Test Case Number: 1
Input: (PolyList [1,2,1,10,5]) 5
Expected Output: 4411
Actual Output: 4411

Function: polyListValue
Test Case Number: 2
Input: (PolyList [-1,0,-5,2,-9]) (-3)
Expected Output: -829
Actual Output: -829

Function: polyListValue
Test Case Number: 3
Input: (PolyList [3]) 5
Expected Output: 3
Actual Output: 3

Function: polyListSum
Test Case Number: 1
Input: (PolyList [2,7,21]) (PolyList [0,1,2,3,0,19,23])
Expected Output: [2,8,23,3,0,19,23]
Actual Output: [2,8,23,3,0,19,23]

Function: polyListSum
Test Case Number: 2
Input: (PolyList [12,4,0]) (PolyList [-5,1,2,-1])
Expected Output: [7,5,2,-1]
Actual Output: [7,5,2,-1]

Function: polyListSum
Test Case Number: 3
Input: (PolyList [0,0,0,-10]) (PolyList [4])
Expected Output: [4,0,0,-10]
Actual Output: [4,0,0,-10]

Function: polyListDegree
Test Case Number: 1
Input: (PolyList [0,0,0,1,1])
Expected Output: 4
Actual Output: 4

Function: polyListDegree
Test Case Number: 2
Input: (PolyList [-1,2,-4])
Expected Output: 2
Actual Output: 2

Function: polyListDegree
Test Case Number: 3
Input: (PolyList [])
Expected Output: 0
Actual Output: 0

Function: polyListProd
Test Case Number: 1
Input: (PolyList [1,2,3]) (PolyList [4,2,1])
Expected Output: [4,10,17,8,3]
Actual Output: [4,10,17,8,3]

Function: polyListProd
Test Case Number: 2
Input: (PolyList [-1,2,-3]) (PolyList [-4,2,1])
Expected Output: [4,-10,15,-4,-3]
Actual Output: [4,-10,15,-4,-3]

Function: polyListProd
Test Case Number: 3
Input: (PolyList [7,0,1,2,3,2]) (PolyList [1,0,3])
Expected Output: [7,0,22,2,6,8,9,6]
Actual Output: [7,0,22,2,6,8,9,6]

Function: polyListToPoly
Test Case Number: 1
Input: (PolyList [1,0,2,3,4])
Expected Output: Sum (Coef 1) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Coef 4))))))))
Actual Output: Sum (Coef 1) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Coef 4))))))))

Function: polyListToPoly
Test Case Number: 2
Input: (PolyList [2,-5,2,9,10])
Expected Output: Sum (Coef 2) (Prod X (Sum (Coef (-5)) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 9) (Prod X (Coef 10))))))))
Actual Output: Sum (Coef 2) (Prod X (Sum (Coef (-5)) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 9) (Prod X (Coef 10))))))))

Function: polyListToPoly
Test Case Number: 3
Input: (PolyList [-3,0,0,5])
Expected Output: Sum (Coef (-3)) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 0) (Prod X (Coef 5))))))
Actual Output: Sum (Coef (-3)) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 0) (Prod X (Coef 5))))))

Function: polyToPolyList
Test Case Number: 1
Input: Sum (Coef 1) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Coef 4))))))))
Expected Output: PolyList [1,0,2,3,4]
Actual Output: PolyList [1,0,2,3,4]

Function: polyToPolyList
Test Case Number: 2
Input: Prod (Coef (-5)) (Sum (Prod (Coef 3) X) X) 
Expected Output: PolyList [0,-20]
Actual Output: PolyList [0,-20]

Function: polyToPolyList
Test Case Number: 3
Input: Prod X (Prod (Coef 3) (Sum X (Coef (-2))))
Expected Output: [0,-6,3]
Actual Output: [0,-6,3]
-}