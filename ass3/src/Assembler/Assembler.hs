{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Assembler.Assembler where
import Parsers.Parser

import Control.Lens
import Control.Monad.State
import Control.Monad.Tardis
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
data M    = M Int Int         deriving Show
newtype E = E Int             deriving Show
data Obj = Obj
  { _hSect :: H
  , _tSect :: [T]
  , _mSect :: [M]
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
toRaw obj = intercalate "\n" $ header : tRecords ++ mRecords ++ [end]
  where
    header = let
      (H name start len) = obj^.hSect
      in printf "H%-6s%06X%06X" name start len
    
    tRecords = let
      ts = obj^.tSect
      tRecord (T loc len bytes) = printf "T%06X%02X" loc len ++ concatMap (printf "%02X") bytes
      in map tRecord ts
    
    mRecords = let
      ms = obj^.mSect
      mRecord (M loc len) = printf "T%06X%01X" loc len
      in map mRecord ms

    end = let
      (E start) = obj^.eSect
      in printf "E%06X" start

type SInt = (Int, Int)
instance (Num a, Num b) => Num (a, b) where
  (+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
  (+) a = bimap (fst a +) (snd a +)
  (*) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
  (*) a = bimap (fst a *) (snd a *)
  abs :: (Num a, Num b) => (a, b) -> (a, b)
  abs = bimap abs abs
  signum :: (Num a, Num b) => (a, b) -> (a, b)
  signum = bimap signum signum
  fromInteger :: (Num a, Num b) => Integer -> (a, b)
  fromInteger i = (fromInteger i, 0) 
  negate :: (Num a, Num b) => (a, b) -> (a, b)
  negate = bimap negate negate

data AssState = AssState
  { _section    :: String
  , _locations  :: Map String SInt
  , _base       :: Maybe SInt
  , _start      :: Int
  , _startName  :: String
  , _end        :: Int
  }
makeLenses ''AssState

data Memory = Memory
  { _memory        :: Map SInt [Word8]
  , _relocation :: Map Int Int
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

-- addLabel :: (Zoom m n (Map String Int) AssState, Functor (Zoomed m ())) => Maybe [Char] -> Int -> n ()
addLabel :: At t => Maybe (Index t) -> IxValue t -> t -> t
addLabel label location = case label of
    Just label -> at label ?~ location
    Nothing    -> id

loc :: Lens' AssState SInt
loc = lens getter setter
  where
    getter :: AssState -> SInt
    getter s = fromMaybe (0,1) $ s ^. (locations . at (s ^. section))

    setter :: AssState -> SInt -> AssState
    setter s val = s { _locations = Map.insert (s^.section) val (s^.locations) }

fromParsed :: [Parsed] -> Obj
fromParsed parsed =
  let 
    t                       = runStateT (mapM assemble parsed) (AssState "" Map.empty Nothing 0 "" 0)
    (memoryState, finalAss) = evalTardis t (Map.empty, Map.empty)
    finalMemory             = execState (sequence memoryState) (Memory Map.empty Map.empty (sum . Map.map fst $ finalAss^.locations))

    -- H
    programName = finalAss^.startName
    codeAddress = finalAss^.start
    codeLength  = finalMemory^.programLength

    -- M
    mChunks = map (uncurry M) (Map.toList $ finalMemory^.relocation)
    
    -- E
    executionStart = finalAss^.end

    splitT :: T -> [T]
    splitT t@(T addr length bytes)
      | length > 30 =
        let (first30, rest) = splitAt 30 bytes
        in T addr 30 first30 : splitT (T (addr+30) (length-30) rest)
      | otherwise   = [t]

    -- T
    tChunks = tFromMap (Map.mapKeysMonotonic fst $ finalMemory^.memory)

    -- M
  in Obj 
    (H programName codeAddress codeLength)
    tChunks
    mChunks
    (E executionStart)

type SymTable = Map String SInt

evaluateExpression :: SInt -> SymTable -> Exp -> SInt
evaluateExpression addr table exp = case exp of
  Val val     -> (val, 0)
  Var var     -> table Map.! var
  Mul e1 e2   -> elemWise (*) (eval e1) (eval e2)
  Div e1 e2   -> elemWise div (eval e1) (eval e2)
  Plus e1 e2  -> elemWise (+) (eval e1) (eval e2)
  Minus e1 e2 -> elemWise (-) (eval e1) (eval e2)
  Star        -> addr
  where
    eval = evaluateExpression addr table
    elemWise op (a, aS) (b, bS) = (a `op` b, aS `op` bS)

requireStartDependent :: SInt -> Int
-- requireStartDependent sint | trace ("sint: " ++ show sint ++ "\n") False = undefined 
requireStartDependent (v,s) = v

requireStartIndependent :: SInt -> Int
requireStartIndependent (v,s) = if s /= 0
  then error "Trying to use start depenent value as a constant"
  else v

assemble :: Parsed -> StateT AssState (Tardis SymTable SymTable) (State Memory ())

-- Blank line
assemble Blank = pure $ pure ()

-- Directive
assemble ( Directive label directive ) = do
  bw <- lift getFuture
  fw <- lift getPast

  l <- use loc
  let table = Map.union bw fw

  case directive of
    EQU exp -> let eval = evaluateExpression l table exp in do
      lift $ modifyBackwards (addLabel label eval)
      lift $ modifyForwards (addLabel label eval)
    _ -> do
      lift $ modifyBackwards (addLabel label l)
      lift $ modifyForwards (addLabel label l)

  case directive of
    BASE exp  -> pure () <$ (base    ?= evaluateExpression l table exp)
    NOBASE    -> pure () <$ (base    .= Nothing)
    START pos -> pure () <$ (start   .= fromIntegral pos >> startName .= fromJust label)
    END exp   -> pure () <$ (end     .= fst (evaluateExpression l table exp))
    ORG exp   -> pure () <$ (loc     .= evaluateExpression l table exp)
    USE sect  -> pure () <$ (section .= sect)
    EQU _     -> pure $ pure ()

-- Storage directive
assemble ( Storage label storage ) = do
  bw <- lift getFuture
  fw <- lift getPast

  l <- use loc
  let table = Map.union bw fw

  lift $ modifyBackwards (addLabel label l)
  lift $ modifyForwards (addLabel label l)
  case storage of
    RESB exp -> pure () <$ (loc += evaluateExpression l table exp)
    RESW exp -> pure () <$ (loc += (3,3) * evaluateExpression l table exp)
    BYTE d -> case d of
      Bytes h -> do
        l <- use loc
        loc += fromIntegral (length h)
        pure $ memory %= Map.union (Map.fromList (zip (map (,snd l) [fst l..]) (map (:[]) h)))
      
      Num n -> do
        l <- use loc
        loc += 1
        pure $ zoom memory $ at l ?= [ n^.byteAt 0 ]
      
    WORD w -> case w of
      Bytes h -> do
        l <- use loc
        loc += fromIntegral (length h)
        pure $ memory %= Map.union (Map.fromList (zip (map (,snd l) [fst l..]) (chunksOf 3 h)))
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
  lift $ modifyBackwards (addLabel label l)
  lift $ modifyForwards (addLabel label l)
  loc += 1
  pure $ zoom memory $ at l ?= [op^.byteAt 0]

-- F2 instruction
assemble ( Instruction label ( F2 op r1 r2 ) ) = do
  l <- use loc
  lift $ modifyBackwards (addLabel label l)
  lift $ modifyForwards (addLabel label l)
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
  lift $ modifyBackwards (addLabel label l)
  lift $ modifyForwards (addLabel label l)

  bw <- lift getFuture
  fw <- lift getPast

  let symtable = Map.union bw fw

  loc += if isExtended then 4 else 3
  l' <- use loc
  pure $ do
    len     <- use programLength
    let
      opWithNi =
        op 
        & bitAt 1 .~ nixbpe^.n 
        & bitAt 0 .~ nixbpe^.i

      xbpeWithOffset =
        (if isExtended 
          then address .>>. 16 
          else address .>>. 8)
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

      requiresRelocation = sAddress /= 0
      (nixbpe, (address, sAddress)) = resolveAddressing f34 symtable _base l' len
      storeOperand       = case operand of
        Annonymous integer -> do
          programLength += fst (l' - l)
          zoom memory $ at (len,1) ?=
              [ integer^.byteAt 2
              , integer^.byteAt 1
              , integer^.byteAt 0
              ]
        _ -> pure ()
    
    when requiresRelocation $ do
      zoom relocation $ do
        at (fst l+1) ?= if isExtended then 6 else 5 

    storeOperand
    zoom memory $ do
      at l ?= (
        [ opWithNi^.byteAt 0
        , xbpeWithOffset ^.byteAt 0
        , offsetTail ^.byteAt 0
        ] ++ [f4Tail^.byteAt 0 | isExtended])

resolveAddressing :: Instruction -> SymTable -> Maybe SInt -> SInt -> Int -> (Nixbpe, SInt)
resolveAddressing instruction symtable base location len = let

  resolveSymbol :: Symbol -> SInt
  resolveSymbol ( Constant int ) = (int, 0)
  resolveSymbol ( Label s )      = case symtable^.at s of
    Nothing  -> error ("Undefined symbol " ++ s)
    Just int -> int
  
  (op, operand, isX, isExtended) = case instruction of
    F3 op operand isX -> (op, operand, isX, False)
    F4 op operand isX -> (op, operand, isX, True)
    _                 -> error "Can only resolve addressing for F3 and F4 instructions"
  
  isConstant = snd address == 0
  ((n,i), address) = case operand of
    NoOperand    -> ((True,  True),  (0,0))
    Simple s     -> ((True,  True),  resolveSymbol s)
    Immediate s  -> ((False, True),  resolveSymbol s)
    Indirect s   -> ((True,  False), resolveSymbol s)
    Annonymous _ -> ((True,  True),  (len,1))

  in case msum $ [ f n i isX isExtended base address location | f <- [tryCONSTANT, tryPC, tryBASE, tryDIRECT] ] of
    Nothing -> error "Could not address symbol"
    Just res -> res

tryCONSTANT :: Bool -> Bool -> Bool -> Bool -> Maybe SInt -> SInt -> SInt -> Maybe (Nixbpe, SInt)
tryCONSTANT n i isX isExtended base (addr, saddr) location
  | saddr /= 0 = Nothing
  | otherwise = let
    inRange = if isExtended
      then addr >= -2^19 && addr <= 2^19-1
      else addr >= -2^11 && addr <= 2^11-1
    in if inRange
      then Just (Nixbpe n i isX False False isExtended, (addr, 0))
      else Nothing

tryPC :: Bool -> Bool -> Bool -> Bool -> Maybe SInt -> SInt -> SInt -> Maybe (Nixbpe, SInt)
tryPC n i isX isExtended base address location
  | isExtended                       = Nothing
  | let 
      (diff, sDiff) = address - location
    in diff > 2^11-1 || diff < -2^11 = Nothing
  | otherwise                        = let 
    (diff, sDiff) = address - location
    in Just (Nixbpe n i isX False True False, (diff .&. 0xFFF, sDiff))

tryBASE :: Bool -> Bool -> Bool -> Bool -> Maybe SInt -> SInt -> SInt -> Maybe (Nixbpe, SInt)
tryBASE n i isX isExtended base address location
  | isExtended = Nothing
  | otherwise =
    case base of
      Nothing -> Nothing
      Just b  -> 
        let (diff, sDiff) = address - b
            size = if isExtended then 2^20-1 else 2^12-1
            mask = if isExtended then 0xFFFFF else 0xFFF
        in if diff < 0 || diff > size
          then Nothing
          else Just (Nixbpe n i isX True False isExtended, (diff .&. mask, sDiff))

tryDIRECT :: Bool -> Bool -> Bool -> Bool -> Maybe SInt -> SInt -> SInt -> Maybe (Nixbpe, SInt)
tryDIRECT n i isX isExtended base (address,sAddress) location
  | isExtended = if fst location < 2^20 
    then Just (Nixbpe n i False False False True, (address .&. 0xFFFFF, sAddress)) 
    else Nothing
  | fst location > 2^15-1 = Nothing
  | otherwise         = Just (Nixbpe False False isX False False False, (address .&. 0x3FFF, sAddress))