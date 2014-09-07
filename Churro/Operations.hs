module Churro.Operations
    ( ChurroOp (..)
    ) where

{-
    Churro operations
    Bools specifcy whether to peek (where appropriate)
-}
data ChurroOp =
      Push Int
    | Pop Bool
    | Add Bool
    | Sub Bool
    | Loop [ChurroOp] Bool Bool
    | Store Bool
    | Load Bool
    | PrintInt Bool
    | PrintChar Bool
    | Read
    | Exit
    deriving (Show, Eq)