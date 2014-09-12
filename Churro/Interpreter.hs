module Churro.Interpreter
    ( interpretAndParse,
      interpret
    ) where

import Data.Char
import Churro.Operations
import Churro.Parser
import Control.Monad.State
import qualified Data.Map.Lazy as Map

{---------------------------------- CONSTANTS ---------------------------------}

minIndex = 0

-- Check if an array value is in bounds
inBounds :: Integer -> Bool
inBounds x = (x >= minIndex)

{--------------------------------- TYPES/DATA ---------------------------------}

{-
    Churro state of (stack, data array), producing a string
-}
type ChurroState a = StateT ([Integer], Map.Map Integer Integer) IO a

{-
    Return type for churro parsing
-}
data ChurroReturn =
      Continue  -- Interpreting should continue
    | Complete  -- Stop interpreting (within current loop)
    | Abort     -- Stop interpreting (passed fully up call chain)

{--------------------------------- OPERATIONS ---------------------------------}

{-
    Print a stack error
-}
stackError :: String -> ChurroState ChurroReturn
stackError opName =
    do{ liftIO $ putStrLn ("Stack error: not enough values for "
                           ++ opName
                           ++ " operation")
      ; return Abort
      }

{-
    Print a data error
-}
dataError :: Integer -> ChurroState ChurroReturn
dataError location =
    do{ liftIO $ putStrLn ("Data error: memory location "
                           ++ (show location)
                           ++ " is out of bounds")
      ; return Abort
      }

{-
    Push a number
-}
push :: Integer -> ChurroState ChurroReturn
push n =
    do{ (stack, array) <- get
      ; put (n:stack, array)
      ; return Continue
      }

{-
    Pop/peek A; discard A
-}
pop :: Bool -> ChurroState ChurroReturn
pop peek =
    do{ (stack, array) <- get
      ; case stack of
            a:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put (newStack, array)
                  ; return Continue
                  }
            
            _    -> stackError "pop"
      }

{-
    Pop/peek A, B; push (B + A)
-}
add :: Bool -> ChurroState ChurroReturn
add peek =
    do{ (stack, array) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b + a):newStack, array)
                  ; return Continue
                  }
            
            _    -> stackError "add"
      }

{-
    Pop/peek A, B; push (B - A)
-}
sub :: Bool -> ChurroState ChurroReturn
sub peek =
    do{ (stack, array) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b - a):newStack, array)
                  ; return Continue
                  }
            
            _    -> stackError "subtract"
      }

{-
    Pop/peek A; if A = 0, jump to end of loop
-}
startLoop :: [ChurroOp] -> Bool -> Bool -> ChurroState ChurroReturn
startLoop ops peekA peekB =
    do{ (stack, array) <- get
      ; case stack of
            a:xs ->
                -- Pop from stack
                do{ let newStack = if peekA then stack else xs
                  ; put (newStack, array)
                  
                  ; if a == 0
                    -- Jump to end of loop
                    then return Continue
                    
                    -- Start loop
                    else do{ ret <- interpret ops
                           ; case ret of
                                 -- Continue loop with second peek value
                                 Complete -> startLoop ops peekB peekB
                                 
                                 -- Stop everything
                                 Abort -> return Abort
                                 
                                 -- This shouldn't happen
                                 Continue ->
                                     do{ liftIO $ putStrLn "Error: unexpected loop continuation"
                                       ; return Abort
                                       }
                           }
                  }
            
            _    -> stackError "begin loop"
      }

{-
    Pop/peek A; if A != 0, continue loop
    Only called inside a loop
-}
endLoop :: Bool -> ChurroState ChurroReturn
endLoop peek =
    do{ (stack, array) <- get
      ; case stack of
            a:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put (newStack, array)
                  ; if a == 0
                    then return Complete
                    else return Continue
                  }
            
            _    -> stackError "end loop"
      }
      
{-
    Pop/peek A, B; store B in memory location A
-}
store :: Bool -> ChurroState ChurroReturn
store peek =
    do{ (stack, array) <- get
      ; case stack of
            a:b:xs ->
                if inBounds b
                then
                    do{ let newStack = if peek then stack else xs
                      ; let newArray = Map.insert a b array
                      ; put (newStack, newArray)
                      ; return Continue
                      }
                else
                    dataError b
            
            _    -> stackError "store"
      }
      
{-
    Pop/peek A; push the value in memory location A
-}
load :: Bool -> ChurroState ChurroReturn
load peek =
    do{ (stack, array) <- get
      ; case stack of
            a:xs ->
                if inBounds a
                then
                    do{ let newStack = if peek then stack else xs
                            loadVal = if (Map.member a array)
                                      then array Map.! a
                                      else 0
                      ; put (loadVal:newStack, array)
                      ; return Continue
                      }
                else
                    dataError a
            
            _    -> stackError "load"
      }

{-
    Pop/peek A; print A as an integer
-}
printInt :: Bool -> ChurroState ChurroReturn
printInt peek =
    do{ (stack, array) <- get
      ; case stack of
            a:xs -> do{ let newStack = if peek then stack else xs
                      ; put (newStack, array)
                      ; liftIO $ print a
                      ; return Continue
                      }
            
            _    -> stackError "print integer"
      }

{-
    Pop/peek A; print A as a character
-}
printChar :: Bool -> ChurroState ChurroReturn
printChar peek =
    do{ (stack, array) <- get
      ; case stack of
            a:xs -> do{ let newStack = if peek then stack else xs
                      ; put (newStack, array)
                      ; liftIO $ putChar $ chr $ fromInteger a
                      ; return Continue
                      }
            
            _    -> stackError "print character"
      }

{-
    Read a single character from stdin and push it to the stack
-}
read :: ChurroState ChurroReturn
read =
    do{ (stack, array) <- get
      ; c <- liftIO $ getChar
      ; let charInteger = toInteger $ ord c;
      ; put (charInteger:stack, array)
      ; return Continue
      }
      
{-
    Exit the program
-}
exit :: ChurroState ChurroReturn
exit =
    do{ return Abort
      }

{-------------------------------- INTERPRETER ---------------------------------}

{-
    Execute the first operation and return whether to continue
-}
execOp :: ChurroOp -> ChurroState ChurroReturn
execOp op =
    do{ case op of
            Push n -> push n
            Pop peek -> pop peek
            Add peek -> add peek
            Sub peek -> sub peek
            Loop ops peekA peekB -> startLoop ops peekA peekB
            Store peek -> store peek
            Load peek -> load peek
            PrintInt peek -> printInt peek
            PrintChar peek -> printChar peek
            Read -> Churro.Interpreter.read
            Exit -> exit
      }
{-
    Interpret Churro code
-}
interpret :: [ChurroOp] -> ChurroState ChurroReturn
interpret (x:xs) =
    -- Execute operations
    do{ ret <- execOp x
      ; (stack, _) <- get
      ; case ret of
            Continue -> interpret xs
            _        -> return ret
      }

interpret _ =
    do{ return Complete }

{-
    Parse and interpret Churro code
-}
interpretAndParse :: String -> String -> IO ()
interpretAndParse input name =
    do{ let ops = parseChurro input name
      ; case ops of
            Right ops -> evalStateT (interpret_ ops) ([], Map.empty)
            Left error -> putStrLn $ "Parse error: " ++ (show error)
      }
  where
    -- Interpret, ignoring continue state
    interpret_ ops =
        do{ _ <- interpret ops
          ; return ()
          }