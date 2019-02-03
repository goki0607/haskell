import qualified Data.Map as Map  

-- data Bool = False | True
--  data means we are defining a new data type
--  & = _ is the type
--  _ = & | & | ... are the constructors, diff
--      values it can take

-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- deriving (Show), this is to make it part of
--   show typeclass so we can print it

-- map (Circle 10 20) [4,5,6,6] 

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)

{- exporting data types in modules
module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where

------ (Shape (..) equals)
 Shape (Rectangle, Circle)
------ Opt not to make any constructor available
 Shape ()
-}

-- Record syntax:
-- order here doesn't matter as long as we list
-- all of them
-- o/w other does matter
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 

-- let guy = Person "a" "b" 6 6.0 "c" "d"
-- retrieveal= firstName guy
-- update= guy {firstName = "e"}

-- type parameters
-- data Maybe a = Nothing | Just a
-- another ex:
-- from this:
{-
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show) 
-}
-- to this:
{-
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)  
-}
-- no real benefit though
-- this:
{-
tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-}
-- vs:
{- tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-}

-- never add typeclass constraints in data
-- declarations!!

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n 

data Person' = Person' { firstName' :: String  
                       , lastName' :: String  
                       , age' :: Int  
                       } deriving (Eq, Show, Read) 

-- can't do: read "Just 't'" :: Maybe a
-- can do  : read "Just 't'" :: Maybe Char

-- data Bool = False | True deriving (Ord)  
-- because False first and True second
-- True is greater than False

-- enum typeclass: things have predecessors
-- and successors
-- bounded typeclass: things have lowest
-- and highest possible values

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- type synoynms (just synonymous no other
-- special meaning)
-- type String = [Char]

-- type PhoneBook = [(String,String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

isPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
isPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]

-- just like we can partially apply functions
-- we can also partially apply type parameters
-- to get new type constructors from them
-- type IntMap v = Map Int v  
-- VS.
-- type IntMap = Map Int 
-- same thing

-- Either a b type
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken then Right code else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

singletonTree :: a -> Tree a
singletonTree val = Node val EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree val EmptyTree = singletonTree val
insertTree val (Node a left right)
  | val == a = Node val left right
  | val < a  = Node a (insertTree val left) right
  | val > a  = Node a left (insertTree val right)

elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree val EmptyTree = False
elemTree val (Node a left right)
  | val == a = True
  | val < a  = elemTree val left
  | val > a  = elemTree val right

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldr (\x acc -> insertTree x acc) EmptyTree

nums = [8,6,4,1,7,3,5]

{-
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  
-}
-- or
{-
class Eq equatable where  
    (==) :: equatable -> equatable -> Bool  
    (/=) :: equatable -> equatable -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  
-}

data TrafficLight = Red | Yellow | Green

-- instance
-- class is for defining new typeclasses
-- instance is for making our types instances
-- of typeclasses

instance Eq TrafficLight where  
  Red == Red = True  
  Green == Green = True  
  Yellow == Yellow = True  
  _ == _ = False

-- minimal complete definition for the typeclass:
-- because == definined in terms of /= and v.v.

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- subclass of other typeclass
{-
class (Eq a) => Num a where  
   ... 
-}

{-
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False 
-}

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

-- id is a standard library function,
-- takes a parameter and returns the same
-- thing

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b 
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

{-
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
-}
{-
instance Functor [] where  
    fmap = map  
-}
{-
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
-}

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

{-
instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x  
-}

class Tofu t where  
    tofu :: j a -> t a j  

data Frank a b  = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where  
    tofu x = Frank x  

data Barry t k p = Barry { yabba :: p, dabba :: t k } 

instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y} 
