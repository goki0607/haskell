applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filterp :: (a -> Bool) -> [a] -> [a]
filterp _ [] = []
filterp f (x:xs)
  | f x       = x : filterp f xs 
  | otherwise = filterp f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDiv :: (Integral a) => a
largestDiv = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x       = x : takeWhile' f xs
  | otherwise = []

collatz :: (Integral a) => a -> [a]
collatz x
  | 1 == x = [1]
  | even x = x:collatz (x `div` 2)
  | odd x  = x:collatz (x*3 +1)

numLongChains :: Int
numLongChains = length (filter cond (map collatz [1..100]))
  where cond x = length x > 15

numLongChains' :: Int
numLongChains' = length (filter (\x -> length x > 15) (map collatz [1..100]))

mapR :: (a -> b) -> [a] -> [b]
mapR f xs = foldr (\x acc -> f x:acc) [] xs

mapL :: (a -> b) -> [a] -> [b]
mapL f xs = foldl (\acc x -> acc ++ [f x]) [] xs

reverseR :: [a] -> [a]
reverseR xs = foldr (\x acc -> acc ++ [x]) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

reverse'' :: [a] -> [a]
reverse'' = foldl (flip' (:)) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{- FUNCTION APPLICATION
($) :: (a -> b) -> a -> b  
f $ x = f x  
-}

{- FUNCTION COMPOSITION 
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  
-}

neg :: (Num a) => [a] -> [a]
neg = map (negate . abs)  

{- ex:
replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
to
replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]}
-}

--fn :: (RealFrac a, Integral c, Floating a) => a -> c
fn :: (RealFrac a, Integral c, Floating a) => a -> c
fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))     

oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  