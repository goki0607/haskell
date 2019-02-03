--reverse' [] = []
--reverse' (x:xs) = (reverse' xs) : [x]

data List a = Cons a (List a) deriving (Show, Read, Eq, Ord)