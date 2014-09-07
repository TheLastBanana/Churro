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

{--------------------------------- OPERATIONS ---------------------------------}

{-
    Print a stack error
-}
stackError :: String -> ChurroState Bool
stackError opName =
    do{ liftIO $ putStrLn ("Stack error: not enough values for "
                           ++ opName
                           ++ " operation!")
      ; return False
      }

{-
    Push a number
-}
push :: Int -> ChurroState Bool
push n =
    do{ (stack, vec) <- get
      ; put (n:stack, vec)
      ; return True
      }

{-
    Pop/peek A; discard A
-}
pop :: Bool -> ChurroState Bool
pop peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put (newStack, vec)
                  ; return True
                  }
            
            _    -> stackError "pop"
      }

{-
    Pop/peek A, B; push (B + A)
-}
add :: Bool -> ChurroState Bool
add peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b + a):newStack, vec)
                  ; return True
                  }
            
            _    -> stackError "add"
      }

{-
    Pop/peek A, B; push (B - A)
-}
sub :: Bool -> ChurroState Bool
sub peek =
    do{ (stack, vec) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peek then stack else xs
                  ; put ((b - a):newStack, vec)
                  ; return True
                  }
            
            _    -> stackError "subtract"
      }

{-
    Pop/peek A; if A = 0, exit loop
    
    At end of each iteration:
    Pop/peek A; if A != 0, continue loop
-}
loop :: [ChurroOp] -> Bool -> Bool -> ChurroState Bool
loop ops peekA peekB =
    do{ (stack, vec) <- get
      ; case stack of
            a:b:xs ->
                do{ let newStack = if peekA then stack else xs
                  ; put (newStack, vec)
                  ; interpret ops
                  }
            
            _    -> stackError "begin loop"
      }

{-------------------------------- INTERPRETER ---------------------------------}

{-
    Execute the first operation and return whether to continue
-}
execOp :: ChurroOp -> ChurroState Bool
execOp op =
    do{ case op of
            Push n -> push n
            Pop peek -> pop peek
            Add peek -> add peek
            Sub peek -> sub peek
      }
{-
    Interpret Churro code
-}
interpret :: [ChurroOp] -> ChurroState Bool
interpret (x:xs) =
    -- Execute operations
    do{ continue <- execOp x
      ; if continue
        then interpret xs
        else return False
      }

interpret _ =
    do{ (stack, _) <- get
      ; liftIO $ putStrLn $ show stack
      ; return False
      }

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