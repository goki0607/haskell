import System.Random  
import Control.Monad(when)
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S    
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen  

{-
import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main  
-}

-- Data.ByteString
-- strict version, each element is one byte
-- or 8 bits in size
-- acts like a list

-- Data.ByteString.Lazy
-- lazy version
-- works in 64K chunks which fits nice into L2
-- cache

-- pack
-- pack :: [Word8] -> ByteString
-- tkaes a list of type Word8 and returns a
-- ByteString

-- Word8
-- range 0-255
-- if larger number wraps around to 80

-- unpack
-- inverse of pack
-- takes a bytesting and turns iti into a
-- list of bytes

-- fromChunks
-- takes a list of strict bytestrings
-- and conversts it to a lazy bytestring

-- toChunks
-- takes a lazy bytestring and converts it
-- to a list of strict ones

-- cons
-- concat a byte and a bytestring
-- lazy, make a new chunk even if the first
-- chunk in the bytestring isn't full

-- cons'
-- strict version of cons'
-- use if a lot of insertions to the begin
-- of a bytestring

-- empty
-- makes empty bytestring

-- similar stuff in Data.List:
-- head
-- tail
-- init
-- null
-- length
-- map
-- reverse
-- foldl
-- foldr
-- concat
-- takeWhile
-- filter

-- readFile
-- read as bytestring