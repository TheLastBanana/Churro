import Churro.Interpreter
import System.Environment
import System.IO

{-
    Opens a file and interprets the Churro code.
-}
main :: IO ()
main =
    do{ args <- getArgs
      ; case args of
            x:xs ->
                do{ handle <- openFile x ReadMode
                  ; code <- hGetContents handle
                  ; parseAndInterpret code x
                  }
            
            _    ->
                do{ putStrLn "Usage: churro <filename>.ch" }
      }