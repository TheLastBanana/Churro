import Churro.Interpreter
import System.Environment
import System.IO

{-
    Opens a file and interprets the Churro code.
-}
main :: IO ()
main =
    do{ args <- getArgs
      ; let file = head args
      ; handle <- openFile file ReadMode
      ; code <- hGetContents handle
      ; interpretAndParse code file
      }