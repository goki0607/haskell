caseTest :: (Integral a) => a -> String
caseTest x
  | x > 5 = "Greater than 5!" ++ word
  | x < 5 = "Less than 5!" ++ word
  | otherwise = "Is 5!" ++ word
  where word = " Word"

printOp :: String -> String
printOp op = "The current operation is " ++ case op of "+" -> "addition."
                                                       "-" -> "subtraction."
                                                       "*" -> "multiplication."
                                                       "/" -> "division."

calculator :: (Fractional a) => a -> a -> String -> a
calculator x y op
  | op == "+" = x + y
  | op == "-" = x - y
  | op == "x" = x * y
  | op == "/" = x / y
  | otherwise = prob
  where prob = error "What?"

action = printOp ("+"::String)

cube :: (Num a) => a -> a -> a -> (a, a)
cube w h l =
  let areaOne = w * h
      areaTwo = h * l
      areaThree = w * l
      widths = 4 * w
      heights = 4 * h
      lengths = 4 * l
  in ( (areaOne + areaTwo + areaThree) * 2, widths + heights + lengths)

{-# LANGUAGE BangPatterns #-}
fib :: (Integral a) => a -> a
fib n =
  tailFib n 1 0
  where tailFib 0 curr prev = prev
        tailFib 1 curr prev = curr
        tailFib n curr prev = tailFib (n-1) (curr+prev) curr

everythree :: (Num a) => [a] -> a -> a
everythree [x,y,z] acc = ((x*y*z)+acc)
everythree (x:y:z:xs) acc = everythree xs ((x*y*z)+acc) 