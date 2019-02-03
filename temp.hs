solve :: String -> [String] -> (String, [String])
solve x xs
  | x == "+"  = (show $ read y + read z, zs)
  | x == "-"  = (show $ read y - read z, zs)
  | x == "*"  = (show $ read y * read z, zs)
  | x == "/"  = (show $ read y / read z, zs)
  | otherwise = ("", [""])
  where (y, ys) = splitAt 1 xs
        (z, zs) = splitAt 1 ys