module Churro.Interpreter
    ( interpretParse
    ) where

import Churro.Operations
import Churro.Parser
import Control.Monad.State
import qualified Data.Vector as V

{--------------------------------- TYPES/DATA ---------------------------------}

{-
    Churro state of (stack, data array), producing a string
-}
type ChurroState a = StateT ([Int], V.Vector Integer) IO a

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
    Push a number
-}
push :: Int -> ChurroState ChurroReturn
push n =
    do{ (stack, vec) <- get
      ; put (n:stack, vec)
      ; return Continue
      }

{-
    Pop/peek A; discard A
-}
pop :: Bool -> ChurroState ChurroReturn
pop peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put (newStack, vec)
                  ; return Continue
                  }
            
            _    -> stackError "pop"
      }

{-
    Pop/peek A, B; push (B + A)
-}
add :: Bool -> ChurroState ChurroReturn
add peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b + a):newStack, vec)
                  ; return Continue
                  }
            
            _    -> stackError "add"
      }

{-
    Pop/peek A, B; push (B - A)
-}
sub :: Bool -> ChurroState ChurroReturn
sub peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b - a):newStack, vec)
                  ; return Continue
                  }
            
            _    -> stackError "subtract"
      }

{-
    Pop/peek A; if A = 0, jump to end of loop
-}
startLoop :: [ChurroOp] -> Bool -> Bool -> ChurroState ChurroReturn
startLoop ops peekA peekB =
    do{ (stack, vec) <- get
      ; case stack of
            a:xs ->
                -- Pop from stack
                do{ let newStack = if peekA then stack else xs
                  ; put (newStack, vec)
                  
                  ; if a == 0
                    -- Jump to end of loop
                    then return Continue
                    
                    -- Start loop
                    else do{ ret <- interpret ops
                           ; case ret of
                                 -- End of loop
                                 Complete -> 
                                     do{ ret <- endLoop peekB
                                       ; case ret of
                                             -- No error; continue loop
                                             Continue -> startLoop ops peekA peekB
                                        
                                             -- No error; stop loop
                                             Complete -> return Continue
                                            
                                             -- Error; stop everything
                                             Abort -> return Abort
                                        }
                                 
                                 -- Error
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
    do{ (stack, vec) <- get
      ; case stack of
            a:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put (newStack, vec)
                  ; if a == 0
                    then return Complete
                    else return Continue
                  }
            
            _    -> stackError "end loop"
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
      }
{-
    Interpret Churro code
-}
interpret :: [ChurroOp] -> ChurroState ChurroReturn
interpret (x:xs) =
    -- Execute operations
    do{ ret <- execOp x
      ; (stack, _) <- get
      ; liftIO $ putStrLn $ show stack
      ; case ret of
            Continue -> interpret xs
            _        -> return ret
      }

interpret _ =
    do{ return Complete }

{-
    Parse and interpret Churro code
-}
interpretParse :: String -> IO ()
interpretParse input =
    do{ let ops = parseChurro input
      ; case ops of
            Right ops -> evalStateT (interpret_ ops) ([], V.replicate 9999 0)
            Left error -> putStrLn $ "Parse error: " ++ (show error)
      }
  where
    -- Interpret, ignoring continue state
    interpret_ ops =
        do{ _ <- interpret ops
          ; return ()
          }