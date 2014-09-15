module Churro.Print
    ( parseAndPrint
    ) where

import Data.Char
import Churro.Operations
import Churro.Parser
import Control.Monad.State

{--------------------------------- TYPES/DATA ---------------------------------}

{-
    Print state of (line width), producing a string
-}
type PrintState a = State Int a

{--------------------------------- FUNCTIONS ----------------------------------}
{-
    Print a single churro given facing, fill and length
-}
printChurro :: Int -> Facing -> Bool -> Integer -> PrintState String
printChurro lineWidth facing filled len =
        -- Produce churro and get length
    do{ let churro = lSide ++ stalk ++ rSide
            cLen = length churro
            
        -- Determine whether to break the line
      ; lineLen <- get
      
      ; if (lineLen + cLen) > lineWidth
      
        -- Break
        then do{ put $ cLen + 1
               ; return $ "\n" ++ churro ++ " "
               }
        
        -- No break
        else do{ put $ lineLen + cLen + 1
               ; return $ churro ++ " "
               }
      }
  where
    makeStalk x | x > 0   = "=" ++ (makeStalk (x - 1))
                | x == 0  = ""
    
    head = "{" ++ (if filled then "*" else "o") ++ "}"
    lSide = if facing == L then head else "{"
    rSide = if facing == R then head else "}"
    stalk = makeStalk len

{-
    Print an array of churros
-}
printCode :: Int -> [ChurroOp] -> PrintState String
printCode lineWidth (x:xs) =
    do{ churroStr <- case x of
            Push n -> printChurro lineWidth L (n < 0) (abs n)
            Pop peek -> printChurro lineWidth R peek 0
            Add peek -> printChurro lineWidth R peek 1
            Sub peek -> printChurro lineWidth R peek 2
            Store peek -> printChurro lineWidth R peek 5
            Load peek -> printChurro lineWidth R peek 6
            PrintInt peek -> printChurro lineWidth R peek 7
            PrintChar peek -> printChurro lineWidth R peek 8
            Read -> printChurro lineWidth R False 9
            Exit -> printChurro lineWidth R False 10
            
            Loop ops peekA peekB ->
                do{ first <- printChurro lineWidth R peekA 3
                  ; inner <- printCode lineWidth ops
                  ; last <- printChurro lineWidth R peekB 4
                  ; return $ first ++ inner ++ last
                  }
      
      ; rest <- printCode lineWidth xs
      ; return $ churroStr ++ rest
      }
              
printCode _ _ =
    do { return "" }
{-
    Parse an input file and output it as clean Churro code
-}
parseAndPrint :: Int -> String -> String -> String
parseAndPrint lineWidth input name =
    do{ let ops = parseChurro input name
      ; case ops of
            Right ops -> evalState (printCode lineWidth ops) 0
            Left error -> "Parse error: " ++ (show error)
      }