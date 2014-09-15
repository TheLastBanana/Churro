import Churro.Print
import System.Environment
import System.IO

{-
    Opens a file and outputs the contained Churro code in pure format (i.e. 80
    characters per line and no comments).
-}
main :: IO ()
main =
    do{ args <- getArgs
      ; case args of
            x:xs ->
                do{ handle <- openFile x ReadMode
                  ; code <- hGetContents handle
                  ; putStrLn $ parseAndPrint 80 code x
                  }
            
            _    ->
                do{ putStrLn "Usage: purify <filename>.ch" }
      }