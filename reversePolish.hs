import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN exp =
  let split = words exp
      ans = foldl (\acc x -> if op x then newList $ solve x acc else [x] ++ acc) [] split
  in read $ head ans


newList :: (String,[String]) -> [String]
newList (x,xs) = x : xs

solve :: String -> [String] -> (String, [String])
solve x xs
  | x == "+"  = (show $ read z' + read y', zs)
  | x == "-"  = (show $ read z' - read y', zs)
  | x == "*"  = (show $ read z' * read y', zs)
  | x == "/"  = (show $ read z' / read y', zs)
  | otherwise = ("", [""])
  where (y, ys) = splitAt 1 xs
        (z, zs) = splitAt 1 ys
        y' = head y
        z' = head z

op :: String -> Bool
op "+" = True
op "-" = True
op "*" = True
op "/" = True
op _ = False 

splitAtChar :: Char -> String -> [String]
splitAtChar char = foldr (\x acc -> if x /= char then [[x]] ++ acc else acc) []

solveRPN' :: String -> Float
solveRPN' = head . foldl fun [] . words
  where fun ys     "sum" = [sum ys]
        fun (x:ys)  "ln" = log x:ys
        fun (x:y:ys) "^" = (y ** x):ys
        fun (x:y:ys) "*" = (x * y):ys
        fun (x:y:ys) "/" = (y / x):ys
        fun (x:y:ys) "+" = (x + y):ys
        fun (x:y:ys) "-" = (y - x):ys
        fun ys numStr    = read numStr:ys