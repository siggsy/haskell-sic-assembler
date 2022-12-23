{-# LANGUAGE OverloadedStrings #-}

module Parsers.Instruction where
import Parsers.Common

import Text.Megaparsec hiding (Label)
import Data.Map (Map, (!?))
import qualified Data.Map as Map

type Address = Int
type Name = String

data Register
  = A | X | L | B | S | T | F
  | RegNum Word
  deriving Show

regAsNum :: Register -> Word
regAsNum reg = case reg of
  A -> 0
  X -> 1
  L -> 2
  B -> 3
  S -> 4
  T -> 5
  F -> 6
  RegNum n -> n

data Instruction
  = F1 Word
  | F2 Word Register Register
  | F3 Word Operand Bool
  | F4 Word Operand Bool
  deriving Show

data Format
  = MnemonicF1
  | MnemonicF2N
  | MnemonicF2R
  | MnemonicF2RN
  | MnemonicF2RR
  | MnemonicF34
  | MnemonicF34O
  deriving (Show, Eq)

data Operand
  = Immediate Symbol
  | Simple Symbol
  | Indirect Symbol
  | Annonymous Int
  | NoOperand
  deriving Show

isAnnonymous :: Operand -> Bool
isAnnonymous (Annonymous _) = True
isAnnonymous _              = False

data Symbol
  = Constant Int
  | Label String
  deriving Show

isLabel :: Symbol -> Bool
isLabel (Label _) = True
isLabel _         = False

firstAsKey :: (a,b,c) -> (a,(b,c))
firstAsKey (a,b,c) = (a,(b,c))

mnemonicMap :: Map String (Word, Format)
mnemonicMap = Map.fromList $ map firstAsKey instructions

instructions :: [(String, Word, Format)]
instructions =
  [ ("LDA",   0x00, MnemonicF34O)
  , ("LDB",   0x68, MnemonicF34O)
  , ("LDCH",  0x50, MnemonicF34O)
  , ("LDF",   0x70, MnemonicF34O)
  , ("LDL",   0x08, MnemonicF34O)
  , ("LDS",   0x6C, MnemonicF34O)
  , ("LDT",   0x74, MnemonicF34O)
  , ("LDX",   0x04, MnemonicF34O)

  , ("STA",   0x0C, MnemonicF34O)
  , ("STB",   0x78, MnemonicF34O)
  , ("STCH",  0x54, MnemonicF34O)
  , ("STF",   0x80, MnemonicF34O)
  , ("STL",   0x14, MnemonicF34O)
  , ("STS",   0x7C, MnemonicF34O)
  , ("STSW",  0xE8, MnemonicF34O)
  , ("STT",   0x84, MnemonicF34O)
  , ("STX",   0x10, MnemonicF34O)

  , ("ADD",   0x18, MnemonicF34O)
  , ("ADDF",  0x58, MnemonicF34O)
  , ("AND",   0x40, MnemonicF34O)
  , ("COMP",  0x28, MnemonicF34O)
  , ("COMPF", 0x88, MnemonicF34O)
  , ("DIV",   0x24, MnemonicF34O)
  , ("DIVF",  0x64, MnemonicF34O)
  , ("MUL",   0x20, MnemonicF34O)
  , ("MULF",  0x60, MnemonicF34O)

  , ("J",     0x3C, MnemonicF34O)
  , ("JEQ",   0x30, MnemonicF34O)
  , ("JGT",   0x34, MnemonicF34O)
  , ("JLT",   0x38, MnemonicF34O)
  , ("JSUB",  0x48, MnemonicF34O)
  , ("WD",    0xDC, MnemonicF34O)
  , ("RD",    0xD8, MnemonicF34O)
  , ("TIX",   0x2C, MnemonicF34O)
  , ("RSUB",  0x4C, MnemonicF34)


  -- F1
  , ("NORM",  0xC8, MnemonicF1)
  , ("FIX",   0xC4, MnemonicF1)
  , ("FLOAT", 0xC0, MnemonicF1)
  , ("HIO",   0xF4, MnemonicF1)
  , ("SIO",   0xF0, MnemonicF1)
  , ("TIO",   0xF8, MnemonicF1)


  -- F2
  , ("ADDR",    0x90, MnemonicF2RR)
  , ("CLEAR",   0xB4, MnemonicF2R)
  , ("COMPR",   0xA0, MnemonicF2RR)
  , ("DIVR",    0x9C, MnemonicF2RR)
  , ("MULR",    0x98, MnemonicF2RR)
  , ("RMO",     0xAC, MnemonicF2RR)
  , ("SHIFTL",  0xA4, MnemonicF2RN)
  , ("SHIFTR",  0xA8, MnemonicF2RN)
  , ("SUBR",    0x94, MnemonicF2RR)
  , ("SVC",     0xB0, MnemonicF2N)
  , ("TIXR",    0xB8, MnemonicF2R)
  ]

regNum :: Parser Register
regNum = RegNum <$> limitedNatural 4

reg :: Parser Register
reg = choice
  [ A <$ char' 'A'
  , X <$ char' 'X'
  , L <$ char' 'L'
  , B <$ char' 'B'
  , S <$ char' 'S'
  , T <$ char' 'T'
  , F <$ char' 'F'
  ]

operand :: Int -> Parser Operand
operand range =
  (string "#" >> Immediate  <$> both)   <|>
  (string "@" >> Indirect   <$> both)   <|>
  (string "=" >> Annonymous <$> const)  <|>
  (string ""  >> Simple     <$> both)
  where
    label = try identifier
    const = try (limitedInteger range)
    both = (Constant <$> const) <|> (Label <$> label)
      

instruction :: Parser Instruction
instruction = do
  mnemonic <- try identifier <?> "mnemonic"
  let
    prefix   = head mnemonic
    isF4     = prefix == '+'
    f34      = if isF4 then F4 else F3
    f34Range = if isF4 then 20 else 15

  case mnemonicMap !? mnemonic of
    Nothing -> fail ("Unknown mnemonic " ++ mnemonic)
    Just (oc, format) -> if isF4 && format /= MnemonicF34O
      then fail "Expected F3/F4 mnemonic"
      else case format of
        MnemonicF1 -> pure $ F1 oc

        MnemonicF2N -> do
          n <- regNum
          pure $ F2 oc n (RegNum 0)
        
        MnemonicF2R -> do
          r <- reg
          pure $ F2 oc r (RegNum 0)

        MnemonicF2RN -> do
          r <- reg
          _ <- comma
          F2 oc r <$> regNum
        
        MnemonicF2RR -> do
          r1 <- reg
          _  <- comma
          F2 oc r1 <$> reg

        MnemonicF34 -> do
          pure $ f34 oc NoOperand False

        MnemonicF34O -> do
          op <- operand f34Range
          isX <- option False (True <$ (comma >> char' 'X'))
          pure $ f34 oc op isX