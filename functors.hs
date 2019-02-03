-- Functors

-- not: instance Functor Either where
-- but: instance Functor (Either a) where
-- and: fmap :: (b -> c) -> Either a b -> Either a c

-- IO (Functor)
{-
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  
-}

-- This
{-
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!" 
-}
-- to this
{-
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
-}

{-
import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
-}

{-
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  
-}
-- if syntax allowed for it:
{-
instance Functor (r ->) where  
    fmap f g = (\x -> f (g x))  
-}
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- function from a to b and function from r to a
-- return a function from r to b
{-
instance Functor ((->) r) where  
    fmap = (.)  
-}
-- i.e. function composition

-- lifting a function
-- fmap :: (a -> b) -> (f a -> f b)
-- (a -> b) function and returns a
-- f a -> f b function

-- two views:
-- 1:
-- fmap a function that takes a function and a functor
-- and then maps the function over the functor
-- 2:
-- a function that takes a function and lifts that 
-- function so that it operates on functors

-- laws of functors:
-- 1:
-- if we map id function over a functor, then we get back
-- the same functor
-- "identity law"
{-
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
-}
-- 2:
-- composing two functions and mapping over a functor
-- is the same as first mapping one function over the
-- functor and then mapping the other one
-- "composition"
-- fmap (f . g) = fmap f . fmap g
-- fmap (f . g) F = fmap f (fmap g F)

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where   
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- applicative functors
-- multi-parameter functions over parameters, get functors
-- that contain functions inside them

-- Applicative typeclass
-- Control.Applicative module
-- two methods: pure and <*>
{-
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b 
-}  