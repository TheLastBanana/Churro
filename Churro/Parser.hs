module Churro.Parser
    ( parseChurro
      
    ) where

import Churro.Operations

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Prim
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Sequence as S

{--------------------------------- TYPES/DATA ---------------------------------}

{-
    Direction of churro facing (L = left, R = right)
-}
data Facing =
      L
    | R
    deriving (Show, Eq)

{-
    Parser for Churro code
-}
type ChurroParser a = Parsec [Char] [ChurroOp] a

{-
    Error or value
-}
type MaybeError a = Either String a
    

{----------------------------------- PARSER -----------------------------------}

{-
    Works like manyTill, but returns a tuple of
    (values, endValue)
    where values is the list of values returned by the parsers and endValue is
    the values returned by the end parser.
-}
manyTillEnd :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTillEnd p end = scan
  where
    scan =
        do{ endr <- end; return ([], endr) }
        <|>
        do{ x <- p; (xs, endr) <- scan; return (x:xs, endr) }

{-
    Definition of whitespace
-}
whiteSpace = many $ oneOf " \n"
    
{-
    Parse a churro filling
-}
filling :: ChurroParser Bool
filling = 
    do{ fill <- oneOf "o*" <?> "filling (\'*\' or \'o\')"
      ; if fill == 'o'
            then return False
            else return True
      }

{-
    Parse the head of a churro and return whether or not it is filled
-}
churroHead :: ChurroParser Bool
churroHead = 
    do{ char '{'
      ; filled <- filling
      ; char '}'
      ; return filled
      }
      
{-
    Parse a churro's tail
-}
churroTail :: ChurroParser Int
churroTail =
    do{ tailStr <- many (char '=')
      ; return $ length tailStr
      }

{-
    Parse a left-facing (literal) churro
-}
lChurro :: ChurroParser (Bool, Int)
lChurro = 
    do{ filled <- churroHead
      ; len <- churroTail
      ; char '}'
      ; whiteSpace
      ; return (filled, len)
      }

{-
    Parse a right-facing (literal) churro
    Return (filled, tail length)
-}
rChurro :: ChurroParser (Bool, Int)
rChurro = 
    do{ char '{'
      ; len <- churroTail
      ; filled <- churroHead
      ; whiteSpace
      ; return (filled, len)
      }

{-
    Parse any churro
    Return (facing, filled, tail length)
-}
anyChurro :: ChurroParser (Facing, Bool, Int)
anyChurro = 
    -- Facing left
    do{ (filled, len) <- try lChurro;
      ; return (L, filled, len)
      }
    <|>
    -- Facing right
    do{ (filled, len) <- try rChurro;
      ; return (R, filled, len)
      }

{-
    Parse a specific type of churro
    If Nothing is given as an argument, then that value is not checked
    
    Return (facing, filled, tail length)
-}
exactChurro :: Maybe Facing -> Maybe Bool -> Maybe Int
               -> ChurroParser (Facing, Bool, Int)
exactChurro facing filled len = 
        -- Get a churro
    do{ (pFacing, pFilled, pLen) <- anyChurro
        -- Is it the right churro?
      ; if foldl1 (&&) [(maybeEq facing pFacing),
                        (maybeEq filled pFilled),
                        (maybeEq len pLen)]
        
        -- Return its info
        then return (pFacing, pFilled, pLen)
        
        -- Failed
        else parserZero <?> "a churro with facing " ++ (show facing)
                            ++ ", fill " ++ (show filled)
                            ++ " and length " ++ (show len)
      }
  where
    -- Nothing -> always match
    -- Just a -> check a = b
    maybeEq :: Eq a => Maybe a -> a -> Bool
    maybeEq a b = maybe True (== b) a

{-
    Decode a single churro
-}
decodeChurro :: ChurroParser ChurroOp
decodeChurro =
    do{ (facing, filled, len) <- anyChurro
      ; op <- case facing of
            -- Literal
            L -> do{ return $ Push (if filled then -len else len) }
            
            -- Operation
            R -> case len of
                0 -> do{ return Pop }
                1 -> do{ return $ Add filled }
                2 -> do{ return $ Sub filled }
                
                -- Decode a loop
                3 -> do{ (ops, (_, endFilled, _)) <- churroLoop
                       ; return $ Loop ops filled endFilled
                       }
                
                -- This won't happen unless a start churro was left out
                4 -> do{ unexpected "end churro" }
                
                5 -> do{ return $ Store filled }
                6 -> do{ return $ Load filled }
                7 -> do{ return $ PrintInt filled }
                8 -> do{ return $ PrintChar filled }
                9 -> do{ return Read }
                10 -> do{ return Exit }
      ; return op
      }
  where
    -- Parse an end churro
    endChurro = exactChurro (Just R) Nothing (Just 4)
    
    -- Parse until end of a loop
    churroLoop = manyTillEnd decodeChurro (try endChurro)

{-
    Parse Churro code
-}
churroCode :: ChurroParser [ChurroOp]
churroCode =
    do{ whiteSpace
      -- Find churros on this line
      ; many $ do
          { state <- getState
          
          -- Decode churro and add to operation list
          ; op <- decodeChurro
          ; setState $ op:state
          }
      ; state <- getState
      ; return $ reverse state
      }
      
  where
    -- Try for an end churro
    tryEnd = try $ exactChurro (Just R) Nothing (Just 4)
  
    -- Skip to next end churro
    jumpAhead = 
        do{ manyTill anyChurro tryEnd }

{-
    Run the churro parser
-}
parseChurro :: String -> Either ParseError [ChurroOp]
parseChurro input = runParser churroCode [] "churro" input
