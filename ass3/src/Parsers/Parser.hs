{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parsers.Parser
  ( ass
  , Operand (Immediate, Simple, Indirect, Annonymous, NoOperand)
  , Symbol (Constant, Label)
  , Parsed (Blank, Instruction, Directive, Storage)
  , Storage (RESB, RESW, BYTE, WORD)
  , Data (Bytes, Num)
  , Directive (BASE, NOBASE, START, END, ORG, USE, EQU)
  , Exp
  , Instruction (F1, F2, F3, F4)
  , regAsNum
  ) 
where

import Parsers.Common
import Parsers.Data
import Parsers.Instruction

-- Parsec
import Text.Megaparsec hiding ( label )
import Text.Megaparsec.Debug

-- Monads
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad
import Data.Maybe

-- Base
import Numeric
import Data.Char
import Data.Int

-- Containers
import Data.Map (Map)
import qualified Data.Map as Map

-- Lens
import Control.Lens

-----------------------------------------
-- Data types ---------------------------
-----------------------------------------

data Storage
  = RESB Exp
  | RESW Exp

  | BYTE Data
  | WORD Data
  deriving Show

data Directive
  = BASE Exp
  | NOBASE
  | START Word
  | END Exp
  | ORG Exp
  | EQU Exp

  | CSECT String
  | USE String
  | EXTDEF [String]
  | EXTREF [String]
  deriving Show

data Parsed
  = Blank
  | Instruction (Maybe String) Instruction
  | Directive (Maybe String) Directive
  | Storage (Maybe String) Storage
  deriving Show

class Labelable a where
  requiresLabel :: a -> Bool

instance Labelable Storage where
  requiresLabel :: Storage -> Bool
  requiresLabel _ = False

instance Labelable Directive where
  requiresLabel :: Directive -> Bool
  requiresLabel (START _) = True
  requiresLabel _ = False

instance Labelable Instruction where
  requiresLabel :: Instruction -> Bool
  requiresLabel _ = False

isBlank :: Parsed -> Bool
isBlank Blank = True
isBlank _     = False

-----------------------------------------
-- Parser combinators -------------------
-----------------------------------------

data Exp 
  = Star
  | Val Int
  | Var String
  | Mul Exp Exp
  | Div Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  deriving Show

term :: Parser Exp
term = (Val <$> try integer <?> "integer")
  <|> (Var <$> try identifier <?> "identifier")
  <|> (Star <$  lexeme (char' '*') <?> "star")
  <|> parenthesised expression

parenthesised :: Parser a -> Parser a
parenthesised = between (char' '(') (char' ')')

expression :: Parser Exp
expression = do
  n <- term
  rest <- optional $ choice
    [ Minus <$> try minus <?> "minus"
    , Plus  <$> try plus  <?> "plus"
    , Mul   <$> try mul   <?> "mul"
    , Div   <$> try div   <?> "div"
    ]
  pure $ maybe n ($ n) rest
  where
    around c = (lexeme (char' c) <?> "operator") *> expression
    plus  = around '+'
    minus = around '-'
    mul   = around '*'
    div   = around '/'

directive :: Parser Directive
directive = identifier >>= \case
  "NOBASE"  -> pure NOBASE
  "BASE"    -> BASE  <$> expression
  "END"     -> END   <$> expression
  "START"   -> START <$> natural
  "ORG"     -> ORG   <$> expression
  "USE"     -> USE   <$> identifier0
  "EQU"     -> EQU   <$> expression
  _         -> fail "Unknown directive"

storage :: Parser Storage
storage = identifier >>= \case
  "RESB" -> RESB <$> expression
  "RESW" -> RESW <$> expression
  "BYTE" -> BYTE <$> rawData 1
  "WORD" -> WORD <$> rawData 3
  _      -> fail "Unknown storage directive"

labeled :: (Labelable a, Show a) => Parser a -> Parser (Maybe String, a)
labeled a = dbg "labeled" $ do
    _label <- label <?> "label"
    let l = if null _label
        then Nothing
        else Just _label
    x <- a
    if requiresLabel x && isNothing l
      then fail "Expected label"
      else pure (l,x)
    
ass :: Parser [Parsed]
ass = filter (not . isBlank) <$> some (choice 
  [ Blank                       <$  try (whiteSpace >> comment <|> void (try . lookAhead $ eol))
  , Instruction <$> fst <*> snd <$> try (labeled instruction)
  , Directive   <$> fst <*> snd <$> try (labeled directive)
  , Storage     <$> fst <*> snd <$> labeled storage
  ] <* (void eol <|> eof)) <* eof
  
-----------------------------------------
-----------------------------------------