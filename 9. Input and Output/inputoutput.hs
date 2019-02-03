--main = putStrLn "hello, world"

{-
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!") 
-}

  -- fetched data must be assigned using
  -- <-- operator
  -- e.g. name <- getLine

  -- in a do block, the last action cannot be bound to a name

{-
import Data.Char  
  
main = do  
  putStrLn "What's your first name?"  
  firstName <- getLine  
  putStrLn "What's your last name?"  
  lastName <- getLine  
  let bigFirstName = map toUpper firstName  
      bigLastName = map toUpper lastName  
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  
-}

main = do   
  line <- getLine  
  if null line  
    then {-do-} return ()  
    else do  
      putStrLn $ reverseWords line  
      main  
    {-
    else (do  
    putStrLn $ reverseWords line  
    main) 
    -}
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 

-- ghc --make helloword
-- ./helloworld
-- OR
-- runhaskell helloworld.hs

-- return in haskell makes an io action
-- out of a pure alue
-- will not cause execution flow to finish

{-
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line 
-}

{-
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b 
-}
-- is equivalent to (and worse than)
{-
main = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b  
-}

-- putStr (no newline)
-- putStrLn (newline)
-- putChar (with chars)
{-
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs 
-}

-- print
-- same thing as putStrLn . show
-- i.e. anything that can be "shown"
{-
main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]  
-}

-- getChar
-- get a char from the console
{-
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  
-}
-- when
-- takes boolean value and an an I/O action
-- returns the same action if True
-- if false it returns return ()
{-
import Control.Monad   
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  
-}
-- sequence
{-
main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
-}
-- to
{-
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs
-}
-- mapping and sequencing:
-- mapM (returns a result)
-- mapM_ (does the same as mapM then throw
-- away thre result, we don't need it)

-- forever, takes an I/O action and returns
-- an I/O action that just repeats the I/O
-- action it got forever

-- forM
-- like mapM only the parameters are switched
-- around, first is list and second is
-- function
{-
import Control.Monad  
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors
    -- forM colors putStrLn
-}
-- forM, map and sequence some actions that we
-- defime there on the spot using do notation

-- getContents
-- read everything from stdin until EOF char
-- is encountered

-- ghc --make capslocker
-- cat haiku.txt
-- cat haiku.txt | ./capslocker

-- shortLinesOnly, split by newline, only take
-- the short lines

-- openFile
-- takes a file path and an IOMode and returns
-- an I/O action that will open a file and
-- handle it as we specifiy

-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  

-- hGetContents
-- takes a Handle and returns IO String
-- getContents directly from stdin
-- hGetContents reads from a given file handle

-- putStr contents
-- prints contents to stdout

-- hClose
-- takes a handle and returns an I/O action
-- that closes the file

-- withFile
-- open, do something (lambda), close

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result

-- hGetLine, hPutStr, hPutStrLn, hGetChar

-- readFile, reads a file
{-
import System.IO  
  
main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents  

-- writeFile, writes to file
{-
import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents) 
-}

-- appendFile
-- appends to end of file instead of writing
-- over it

{-
import System.IO     
    
main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n") 
-}

{-
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)  
-}

-- hSetBuffering, takes handle and a BufferMode
-- an returns an I/O actions that sets the
-- buffering
-- data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

{-
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)  
-}

-- hFlush
-- take a handle and flush the buffer of the
-- file associated with the handle

-- openTempFile,
-- open a temporary file

-- removeFile,
-- removes a file using a FilePath

-- renameFile
-- renames a file using a FilePath

