{-
import System.Environment  
import System.IO  
import System.Directory  
  
main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!"
-}

-- exceptions
-- keep to a minimum in pure code
-- doesFileExist
-- doesFileExist :: FilePath -> IO Boo

-- from System.IO.Error
-- catch
-- catch :: IO a -> (IOError -> IO a) -> IO a

import System.Environment     
import System.IO     
import System.IO.Error     
    
main = toTry `catch` handler     
                 
toTry :: IO ()     
toTry = do (fileName:_) <- getArgs     
           contents <- readFile fileName     
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     
    
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e 

-- isDoesNotExistError
-- ioError
-- isAlreadyExistsError
-- isDoesNotExistError
-- isAlreadyInUseError
-- isFullError
-- isEOFError
-- isIllegalOperation
-- isPermissionError
-- isUserError 

-- remember to rethrow exception
-- or else it will fail where it shouldn't

-- ioeGetFileName
-- ioeGetFileName :: IOError -> Maybe FilePath

{-
main = do toTry `catch` handler1  
          thenTryThis `catch` handler2  
          launchRockets  
-}
