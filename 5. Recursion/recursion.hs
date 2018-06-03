maximum' :: (Ord a) => [a] -> a
maximum' [] = error "What does that mean?"
maximum' [x] = x
{-
maximum' (x:xs)
  | x > maxTail = x
  | otherwise   = maxTail
  where maxTail = maximum' xs
-}
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse'  [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: (Num i, Ord i) => i ->  a -> [a]
repeat' n x 
  | n <= 0    = []
  | otherwise = x:repeat' (n-1) x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
{-
elem' x (y:ys)
  | ys == []  = False
  |  x == y   = True 
  | otherwise = elem' x ys
-}
elem' x [] = False
elem' x (y:ys)
  | x == y    = True
  | otherwise = x `elem'` ys

{- quicksort -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

{- first mergesort attempt
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | y < x     = y : merge (x:xs) (ys)
  | otherwise = x : merge (xs) (y:ys)

{- mergesort -}
mergesort :: (Ord a) => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort (x:y:[]) = if y > x then [x,y] else [y,x]
mergesort xs =
  let middle = floor ( fromIntegral (length xs) / 2)
      left   = mergesort (slice 0 middle xs)
      right  = mergesort (slice (middle+1) (length xs) xs)
  in merge left right
-}

{- mergesort better -}
split :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | y < x     = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort xs =
  let (left, right) = split xs
  in merge (mergesort left) (mergesort right)

{- bubblesort -}
swap :: (Ord a) => [a] -> [a]
swap [x] = [x]
swap (x:y:xs)
  | y > x     = x : swap (y:xs)
  | otherwise = y : swap (x:xs)

bubblesort' :: (Ord a) => Int -> [a] -> [a]
bubblesort' n xs
  | n == (length xs) = xs
  | otherwise        = bubblesort' (n+1) (swap xs)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' 0 xs

{- insertion sort -}
partition :: (Ord a) => Int -> [a] -> ([a], [a])
partition n xs = (take (n-1) xs, drop n xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | y > x     = x:y:ys
  | otherwise = y:insert x ys


insertionsort' :: (Ord a) => Int -> [a] -> [a]
insertionsort' n xs
  | n == (length xs) + 1 = xs
  | otherwise            = insertionsort' (n+1) ((insert key left) ++ right)
  where key = xs!!(n-1)
        (left, right) = partition n xs

insertionsort :: (Ord a) => [a] -> [a]
insertionsort xs = insertionsort' 1 xs

{- selection sort -}
{- stupid minimum
minimum' :: (Ord a) => Int -> [a] -> (Int, a)
minimum' n [x] = (n, x)
minimum' n (x:xs)
  | x < snd minTail = (n, x)
  | otherwise       = minTail
  where minTail = minimum' (n+1) xs
-}
minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

delete :: (Ord a) => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
  | x == y    = delete x ys
  | otherwise = y : delete x ys

selectionsort :: (Ord a) => [a] -> [a]
selectionsort [] = []
selectionsort xs = m : selectionsort tl
  where m = minimum xs
        tl = delete m xs