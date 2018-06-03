import Data.List
import qualified Data.Function as F
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube   

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

{- GHCI>
:m + Data.List  
:m + Data.List Data.Map Data.Set  
-}

{- import ONLY
import Data.List (nub, sort)
-}

{- import BUT
import Data.List hiding (nub)
-}

{- import WITHOUT NAMING CONFLICTS
import qualified Data.Map
THIS MEANS WE HAVE
filter f xs (from Prelude)
Data.Map.filter f xs (from Data.Map)
-}

{- import WITHOUT NAMING CONFLICTS +
ASSIGN NAME TO IMPORTED MODULES
import qualified Data.Map as M  
THIS MEANS WE HAVE
filter f xs (from Prelude)
M.filter f xs (from Data.Map)
-}

{- Data.List module
intersperse
  intersperse :: a -> [a] -> [a]
  takes an elem and list, puts elem between each elem in list
intercalate
  intercalate :: [a] -> [[a]] -> [a]
  takes a list and lits of lists, insert and flatten
transpose
  transpose :: [[a]] -> [[a]]
  transpose a 2D array
foldl' and foldl1'
  strict versions of their lazy counterparts
  avoid stack overflow (since lazy will not actually compute until asked to)
concat
  concat :: Foldable t => t [a] -> [a]
  flatten a list of lists into a list
concatMap
  concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
  first mapping a function to a list, then concat
and
  and :: Foldable t => t Bool -> Bool
  list of boolean values and returns True if all are True
or
  or :: Foldable t => t Bool -> Bool
  like and but or
any
  any :: Foldable t => (a -> Bool) -> t a -> Bool
  if any of the elements satisfy a predicate
all
  all :: Foldable t => (a -> Bool) -> t a -> Bool
  if all of the elements satisfy a predicate
iterate
  iterate :: (a -> a) -> a -> [a]
  take a function and starting value, apply over and over again
splitAt
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt some index as specified
takeWhile
  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile predicate is true
dropWhile
  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile preidcate is false
span
  span :: (a -> Bool) -> [a] -> ([a], [a])
  pair: (takeWhile, rest)
break
  break :: (a -> Bool) -> [a] -> ([a], [a])
  break when predicate is true
  break p ====== span (not . p) 
sort
  sort :: Ord a => [a] -> [a]
  sorts
group
  group :: Eq a => [a] -> [[a]]
  takes a list and groups adjacten elements
inits
  inits :: [a] -> [[a]]
  like init but recursively applied
tails
  tails :: [a] -> [[a]]
  like taile but recursively appled
isInfixOf
  isInfixOf :: Eq a => [a] -> [a] -> Bool
  see search below, a `isInfixOf` b
isPrefixOf
  isPrefixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf
  isSuffixOf :: Eq a => [a] -> [a] -> Bool
elem
  elem :: (Eq a, Foldable t) => a -> t a -> Bool
notElem
  notElem :: (Eq a, Foldable t) => a -> t a -> Bool
partition
  partition :: (a -> Bool) -> [a] -> ([a], [a])
  (satisfy predicate, not satisy predicate)
find
  find :: Foldable t => (a -> Bool) -> t a -> Maybe a
  returns Just (smthn) or Nothing if pred is not true
elemIndex
  elemIndex :: Eq a => a -> [a] -> Maybe Int
  is a maybe version of `elem`, finds if there and gives indx
elemIndices
  elemIndices :: Eq a => a -> [a] -> [Int]
findIndex
  findIndex :: (a -> Bool) -> [a] -> Maybe Int
  tries to find index of predicate, maybe
zip3, zip4, ... zip7 (zip n amount of lists together)
lines
  lines :: String -> [String]
  splt by newline character
unlines
  unlines :: [String] -> String
  inverse function of lines
words (split by spaces) and unwords (inversde words)
delete
  delete :: Eq a => a -> [a] -> [a]
  element and list, delete first occurence of element from list
\\
  (\\) :: Eq a => [a] -> [a] -> [a]
  list difference function, every element in right remove matching
  one from left
union
  union :: Eq a => [a] -> [a] -> [a]
  set union
intersect
  intersect :: Eq a => [a] -> [a] -> [a]
  set intersection
insert
  insert :: Ord a => a -> [a] -> [a]
  element and list, insert elem to first place ge in list
generic versions (gives a Num):
  genericLength,
  genericTake,
  genericDrop,
  genericSplitAt, 
  genericIndex,
  genericReplicate
generic by versions:
  nubBy,
  deleteBy,
  unionBy,
  intersectBy,
  groupBy
  The difference is normal use == for equality testing
  while these use a predicate as specified by user
from Data.Function we can get 
  import qualified Data.Function as F
  F.on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  f `on` g = \x y -> f (g x) (g y)
  (==) `on` (> 0) gives \x y -> (x > 0) == (y > 0)
more generics:
  sortBy,
  insertBy,
  maximumBy,
  minimumBy
  sortBy :: (a -> a -> Ordering) -> [a] -> [a]
  sort is the same as sortBy compare
usually:
  By functions that take an equality function,
    you usually do (==) `on` something
  and when you're dealing with By functions that take an ordering function,
    you usually do compare `on` something.
-}

search :: (Eq a) => [a] -> [a] -> Bool
search n h =
  let nlen = length n
  in foldl (\acc x -> if take nlen x == n then True else acc) False (tails h)

stupid :: (Ord a, Show a) => [a] -> [a]
stupid = sortBy (compare `F.on` length . show)

{- Data.Char module
isControl
isSpace
isLower
isUpper
isAlpha
isAlphaNum
isPrint
isDigit
isOctDigit
isHexDigit
isLetter
isMark
isNumber
isPunctuation
isSymbol
isSeperator
isAscii
isLatin1
isAsciiUpper
isAsciiLower
  name :: Char -> Bool
generalCategory
  generalCategory :: Char -> GeneralCategory
  a general cateogry (data type) introduced by module
toUpper
toLower
toTitle
digitToInt
intToDigit
ord and chr (ord to number and char to character)
-}

words' :: [Char] -> [[Char]]
words' = filter (not . any C.isSpace) . groupBy ((==) `F.on` C.isSpace)

encode :: Int -> String -> String
encode shift msg =
  let ords = map C.ord msg
      shifted = map (+ shift) ords
  in map C.chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

{- Data.Map module
fromList
  takes an association list and gives back according map
  Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v  
empty
  empty map
insert
  takes a key, value and map and returns new map
null
  checks if map is empty
size
  size of map
singleton
  key and value, map with single mapping
lookup
  works like Data.list lookup but on maps
  Just v or Nothing (found/not found)
member
  key and map, true if key in map
map and filter
  just like their list equivalents
toList
  inverse of fromList
keys
  returns list of keys
  keys = map fst . Map.toList
elems
  returns list of values
  elems = map snd . Map.toList
fromListWith
  like fromList, but decides what to do with duplicates
  uses some function
insertWith
  like insert, but decides what to do with duplicates
  again using some function
-}

lookupKey :: (Eq k) => k -> [(k,v)] -> Maybe v
lookupKey key [] = Nothing
lookupKey key ((k,v):xs)
  | key == k  = Just v
  | otherwise = lookupKey key xs

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

lookupKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookupKey' key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty

phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ] 

phoneBookToMap :: (Ord k) => [(k, String)] -> M.Map k String  
phoneBookToMap xs = M.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMap' :: (Ord k) => [(k, a)] -> M.Map k [a]  
phoneBookToMap' xs = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

{- Data.Set module
fromList
  takes a list and converts to set
  elements are unique and ordered
intersect
  takes two sets and checks for intersection
difference
  takes two sets and checks for arg1 \ arg2
union
  union of two sets 
null
  checks if set is null
size
  size of seet
member
  checks if element is member of set
empty
  creates an empty set
singleton
  creates a singleton set
insert
  inserts into set
delete
  deletes from set
isSubsetOf
  checks if set1 is subset of set2 with infix notation
  set1 `Set.isSubsetOf` set2
isProperSubset
  checks if set is proper subset of set2
  set1 `Set.isProperSubsetOf` set2
map and filter with sets
toList
  inverse fromList
  people use in place of nub to weed out duplicates in lists
  toList . fromList $ xs (toList $ fromList xs)
  however nub requires only Eq while Sets require Ord
-}






