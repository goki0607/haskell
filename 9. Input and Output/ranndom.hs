-- System.Random module

-- random
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- RandomGen typecless is for types that can
-- act as sources of randomness

-- StdGen is one type of random value
-- mkStdGen to manually make a random generator
-- mkStdGen :: Int -> StdGen

-- random (mkStdGen 100)
-- NOPE, this:
-- random (mkStdGen 100) :: (Int, StdGen) 
-- random (mkStdGen 949494) :: (Int, StdGen) 
-- random (mkStdGen 949488) :: (Float, StdGen)  
-- random (mkStdGen 949488) :: (Bool, StdGen)
-- random (mkStdGen 949488) :: (Integer, StdGen)

import System.Random
import Data.List 

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin) 

-- randoms function, takes a generator and
-- returns an infinite sequence of values
-- based on that generator

{-
take 5 $ randoms (mkStdGen 11) :: [Int]
take 5 $ randoms (mkStdGen 11) :: [Bool] 
take 5 $ randoms (mkStdGen 11) :: [Float] 
-} 

randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

-- randomR
-- randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
-- returns random number within a range

{-
randomR (1,6) (mkStdGen 359353) 
randomR (1,6) (mkStdGen 35935335) 
-}

-- randomRs
-- produces a stream of random variables within
-- a range
-- take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]  

-- getStdGen

{-
main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)
-}

{-
import System.Random  
  
main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen2 <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen2)
-}  
   
main = do  
    gen <- getStdGen  
    let randomChars = randomRs ('a','z') gen  
        (first20, rest) = splitAt 20 randomChars  
        (second20, _) = splitAt 20 rest  
    putStrLn first20  
    putStr second20  

-- newStdGen
-- splits current gen into two gens
-- updates global gen with one and 
-- encapsulates the other as its result

{-
import System.Random  
  
main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen
-}

-- reads
-- empty list when fails
-- when succeeds it returns a singleton
-- list with a tuple that has our desired
-- value as one component and a string
-- with what it didn't consume as the
-- other