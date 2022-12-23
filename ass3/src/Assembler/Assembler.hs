{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Assembler.Assembler where
import Parsers.Parser

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.List

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Bits
import Data.Bits.Lens
import Data.Word
import Data.Char
import Numeric

import Debug.Trace
import Text.Printf

data Nixbpe = Nixbpe
  { _n :: Bool
  , _i :: Bool
  , _x :: Bool
  , _b :: Bool
  , _p :: Bool
  , _e :: Bool
  } deriving Show
makeLenses ''Nixbpe

data H    = H String Int Int  deriving Show
data T    = T Int Int [Word8]
newtype E = E Int             deriving Show
data Obj = Obj
  { _hSect :: H
  , _tSect :: [T]
  , _eSect :: E
  }
makeLenses ''Obj

instance Show T where
  show :: T -> String
  show (T loc len bytes) = "T " ++ printf "%06X" loc ++ "  " ++ printf "%02X" len ++ "  " ++ concatMap (printf "%02X ") bytes

instance Show Obj where
  show :: Obj -> String
  show obj = show (obj^.hSect) ++ "\n" ++ intercalate "\n" (map show (obj^.tSect)) ++ "\n" ++ show (obj^.eSect)

toRaw :: Obj -> String
toRaw obj = intercalate "\n" $ header : tRecords ++ [end]
  where
    header = let
      (H name start len) = obj^.hSect
      in printf "H%-6s%06X%06X" name start len
    
    tRecords = let
      ts = obj^.tSect
      tRecord (T loc len bytes) = printf "T%06X%02X" loc len ++ concatMap (printf "%02X") bytes
      in map tRecord ts
    
    end = let
      (E start) = obj^.eSect
      in printf "E%06X" start
      


data AssState = AssState
  { _section    :: String
  , _locations  :: Map String Int
  , _base       :: Maybe String
  , _labels     :: Map String Int
  , _start      :: Int
  , _startName  :: String
  , _end        :: String
  , _relocation :: Map Int Int
  }
makeLenses ''AssState

data Memory = Memory
  { _memory        :: Map Int [Word8]
  , _finalLabels   :: Map String Int
  , _programLength :: Int
  }
makeLenses ''Memory

tFromMap :: Map Int [Word8] -> [T]
tFromMap memory = go (Map.toAscList memory)
  where
    go [] = []
    go m = let
      address = fst $ head m
      (bytes, rest) = span (\(l, bytes) -> l - address + length bytes <= 30) m
      bytes' = concatMap snd bytes
      len = length bytes'
      in T address len bytes' : go rest

addLabel :: (Zoom m n (Map String Int) AssState, Functor (Zoomed m ())) => Maybe [Char] -> Int -> n ()
addLabel label location = zoom labels ( case label of
    Just label -> at label ?= location
    Nothing    -> pure () )

loc :: Lens' AssState Int
loc = lens getter setter
  where
    getter :: AssState -> Int
    getter s = fromMaybe 0 $ s ^. (locations . at (s ^. section))

    setter :: AssState -> Int -> AssState
    setter s val = s { _locations = Map.insert (s^.section) val (s^.locations) }

fromParsed :: [Parsed] -> Obj
fromParsed parsed =
  let 
    (memoryState, finalAss) = runState  (mapM assemble parsed) (AssState "" Map.empty Nothing Map.empty 0 "" "" Map.empty)
    finalMemory             = execState (sequence memoryState) (Memory Map.empty (finalAss^.labels) (maximum $ finalAss^.locations))

    -- H
    programName = finalAss^.startName
    codeAddress = finalAss^.start
    codeLength  = finalMemory^.programLength
    
    -- E
    executionStart = fromMaybe 0 $ finalAss^.labels.at (finalAss^.end)

    splitT :: T -> [T]
    splitT t@(T addr length bytes)
      | length > 30 =
        let (first30, rest) = splitAt 30 bytes
        in T addr 30 first30 : splitT (T (addr+30) (length-30) rest)
      | otherwise   = [t]

    -- T
    tChunks = tFromMap (finalMemory^.memory)

  in Obj 
    (H programName codeAddress codeLength)
    tChunks
    (E executionStart)

assemble :: Parsed -> State AssState (State Memory ())

-- Blank line
assemble Blank = pure $ pure ()

-- Directive
assemble ( Directive label directive ) = do
  l <- use loc
  addLabel label l
  case directive of
    BASE location -> pure () <$ (base    .= Just location)
    NOBASE        -> pure () <$ (base    .= Nothing)
    START pos     -> pure () <$ (start   .= fromIntegral pos >> startName .= fromJust label)
    END location  -> pure () <$ (end     .= location)
    ORG pos       -> pure () <$ (loc     .= fromIntegral pos)
    USE sect      -> pure () <$ (section .= sect)
    EQU _         -> undefined

-- Storage directive
assemble ( Storage label storage ) = do
  l <- use loc
  addLabel label l
  case storage of
    RESB size -> pure () <$ (loc += fromIntegral size)
    RESW size -> pure () <$ (loc += 3 * fromIntegral size)
    BYTE d -> case d of
      Bytes h -> do
        l <- use loc
        loc += fromIntegral (length h)
        pure $ memory %= Map.union (Map.fromList (zip [l..] (map (:[]) h)))
      
      Num n -> do
        l <- use loc
        loc += 1
        pure $ zoom memory $ at l ?= [ n^.byteAt 0 ]
      
    WORD w -> case w of
      Bytes h -> do
        l <- use loc
        loc += fromIntegral (length h)
        pure $ memory %= Map.union (Map.fromList (zip [l..] (chunksOf 3 h)))
      Num n -> do
        l <- use loc
        loc += 3
        pure $ zoom memory $ do
          at l ?= 
            [ n^.byteAt 2
            , n^.byteAt 1
            , n^.byteAt 0
            ]
    where
      chunksOf _ [] = []
      chunksOf n bs = let (b,bss) = splitAt n bs in b : chunksOf n bss


-- F1 instruction
assemble ( Instruction label ( F1 op ) ) = do
  l <- use loc
  addLabel label l
  loc += 1
  pure $ zoom memory $ at l ?= [op^.byteAt 0]

-- F2 instruction
assemble ( Instruction label ( F2 op r1 r2 ) ) = do
  l <- use loc
  addLabel label l
  loc += 2
  pure $ zoom memory $ do
    at l ?= [ op^.byteAt 0, (regAsNum r1 .<<. 4 .|. regAsNum r2)^.byteAt 0 ] 

-- F3/F4 instruction
assemble ( Instruction label f34 ) = do
  let
    (op, operand, isX, isExtended) = case f34 of
      F3 op operand isX -> (op, operand, isX, False)
      F4 op operand isX -> (op, operand, isX, True)
  l <- use loc
  _base <- use base
  addLabel label l
  loc += if isExtended then 4 else 3
  l' <- use loc
  pure $ do
    len     <- use programLength
    _labels <- use finalLabels
    let
      opWithNi =
        op 
        & bitAt 1 .~ nixbpe^.n 
        & bitAt 0 .~ nixbpe^.i

      xbpeWithOffset =
        if isExtended 
          then address .>>. 16 
          else address .>>. 8
        &~ do
          bitAt 7 .= nixbpe^.x
          bitAt 6 .= nixbpe^.b
          bitAt 5 .= nixbpe^.p
          bitAt 4 .= nixbpe^.e

      offsetTail = 
        if isExtended 
          then address .>>. 8 
          else address

      f4Tail = address

      resolveSymbol :: Symbol -> Int
      resolveSymbol ( Constant int ) = int
      resolveSymbol ( Label s )      = case _labels^.at s of
        Nothing  -> error ("Undefined symbol " ++ s)
        Just int -> int

      resolveLabel :: String -> Int
      resolveLabel s = resolveSymbol ( Label s )

      baseAddress = resolveLabel <$> _base
      ((nixbpe, address), storeOperand) = case operand of
        NoOperand          -> ((Nixbpe True True False False False False, 0), pure())
        Immediate symbol   -> (resolveAddressing I  baseAddress isX isExtended (resolveSymbol symbol) l', pure ())
        Simple symbol      -> (resolveAddressing S  baseAddress isX isExtended (resolveSymbol symbol) l', pure ())
        Indirect symbol    -> (resolveAddressing In baseAddress isX isExtended (resolveSymbol symbol) l', pure ())
        Annonymous integer -> (resolveAddressing S  baseAddress isX isExtended len l', do
          programLength += l' - l
          zoom memory $ at len ?=
              [ integer^.byteAt 2
              , integer^.byteAt 1
              , integer^.byteAt 0
              ])

    storeOperand
    zoom memory $ do
      at l ?= (
        [ opWithNi^.byteAt 0
        , xbpeWithOffset ^.byteAt 0
        , offsetTail ^.byteAt 0
        ] ++ [f4Tail^.byteAt 0 | isExtended])

data Addressing = I | S | In

resolveAddressing :: Addressing -> Maybe Int -> Bool -> Bool -> Int -> Int -> (Nixbpe, Int)
resolveAddressing addressing base isX isExtended address location = case addressing of
  I -> (Nixbpe False True False False False isExtended, address)

  S -> case msum $ [ f True True isX isExtended base address location | f <- [tryPC, tryBASE, tryDIRECT] ] of
    Nothing -> error "Could not address symbol"
    Just res -> res

  In -> case msum $ [ f True False isX isExtended base address location | f <- [tryPC, tryBASE, tryDIRECT] ] of
    Nothing -> error "Could not address symbol"
    Just res -> res

tryPC :: Bool -> Bool -> Bool -> Bool -> Maybe Int -> Int -> Int -> Maybe (Nixbpe, Int)
tryPC n i isX isExtended base address location
  | isExtended                       = Nothing
  | let 
      diff = address - location
    in diff > 2^11-1 || diff < -2^11 = Nothing
  | otherwise                        = Just (Nixbpe n i isX False True False, (address - location) .&. 0x3FFF)

tryBASE :: Bool -> Bool -> Bool -> Bool -> Maybe Int -> Int -> Int -> Maybe (Nixbpe, Int)
tryBASE n i isX isExtended base address location = case base of
  Nothing -> Nothing
  Just b  -> 
    let diff = address - b
        size = if isExtended then 2^20-1 else 2^12-1
    in if diff < 0 || diff > size
      then Nothing
      else Just (Nixbpe n i isX True False False, diff .&. 0x3FFFFF)

tryDIRECT :: Bool -> Bool -> Bool -> Bool -> Maybe Int -> Int -> Int -> Maybe (Nixbpe, Int)
tryDIRECT n i isX isExtended base address location
  | location > 2^15-1 = Nothing
  | otherwise         = Just (Nixbpe False False isX False False False, address)