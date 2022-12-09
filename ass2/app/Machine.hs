{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Machine where
import Data.Int
import Data.Word
import Data.Bits
import Data.List
import Data.List.Split
import GHC.Float
import Data.Map (Map, empty, lookup)
import qualified Data.Map as M
import Control.Exception
import GHC.Prim
import Data.Maybe
import Data.Profunctor
-- import Control.Newtype
import Text.Printf
import GHC.IO.StdHandles
import GHC.IO.IOMode
import GHC.IO.Handle
import Control.Monad
import Foreign
import qualified Control.Monad.State.Strict as S
import Data.Void
import Control.Comonad.Representable.Store

import Control.Lens

-- Type definitions -----------------------------------------

maxAddress :: Word32
maxAddress = 2^20-1

newtype SicByte     = SicByte Int deriving (Num, Ord, Eq, Show, PrintfArg, Enum, Integral, Real, Bits)
newtype SicWord     = SicWord Int deriving (Num, Ord, Eq, Show, PrintfArg, Enum, Integral, Real, Bits)
newtype SicFloat    = SicFloat Double deriving (Num, Ord, Eq, Show, PrintfArg, Enum, Real, Floating, Fractional, RealFrac) 
type Address        = SicWord
type Memory         = Map Address SicByte

-- Registers ------------------------------------------------

data Size a = Bytes Int | Bits Int deriving Show

instance FiniteBits SicByte where
    finiteBitSize :: SicByte -> Int
    finiteBitSize _ = 8

instance FiniteBits SicWord where
    finiteBitSize :: SicWord -> Int
    finiteBitSize _ = 24

instance FiniteBits SicFloat where
    finiteBitSize :: SicFloat -> Int
    finiteBitSize _ = 48

size :: forall a. (FiniteBits a) => Size a
size = Bits $ finiteBitSize @a undefined

bits :: (FiniteBits a) => Size a -> Int
bits s = case s of
    Bits bitCount -> bitCount
    Bytes byteCount -> byteCount * 8

bytes :: (FiniteBits a) => Size a -> Int
bytes s = case s of
    Bits bitCount -> bitCount `div` 8
    Bytes byteCount -> byteCount

instance Bits SicFloat where
    bitSize :: SicFloat -> Int
    bitSize a = 48

    (.&.) :: SicFloat -> SicFloat -> SicFloat
    (.&.) (SicFloat a) (SicFloat b) = SicFloat $ castWord64ToDouble (castDoubleToWord64 a .&. castDoubleToWord64 b)

    (.|.) :: SicFloat -> SicFloat -> SicFloat
    (.|.) (SicFloat a) (SicFloat b) = SicFloat $ castWord64ToDouble (castDoubleToWord64 a .|. castDoubleToWord64 b)

    xor :: SicFloat -> SicFloat -> SicFloat
    xor (SicFloat a) (SicFloat b) = SicFloat $ castWord64ToDouble (xor (castDoubleToWord64 a) (castDoubleToWord64 b))

    shift :: SicFloat -> Int -> SicFloat
    shift (SicFloat a) b = SicFloat $ castWord64ToDouble (shift (castDoubleToWord64 a) b)

    complement :: SicFloat -> SicFloat
    complement (SicFloat a) = SicFloat $ castWord64ToDouble (complement . castDoubleToWord64 $ a)

    rotate :: SicFloat -> Int -> SicFloat
    rotate (SicFloat a) b = SicFloat $ castWord64ToDouble (rotate (castDoubleToWord64 a) b)
    
    bitSizeMaybe :: SicFloat -> Maybe Int
    bitSizeMaybe = Just . finiteBitSize

    isSigned :: SicFloat -> Bool
    isSigned = isSigned

    testBit :: SicFloat -> Int -> Bool
    testBit (SicFloat a) b = testBit word (b + diff)
        where
            word = castDoubleToWord64 a
            diff = bits @Word64 size - bits @SicFloat size

    bit :: Int -> SicFloat
    bit a = SicFloat $ castWord64ToDouble (bit (a + diff))
        where
            diff = bits @Word64 size - bits @SicFloat size
    
    popCount :: SicFloat -> Int
    popCount (SicFloat a) = popCount (castDoubleToWord64 a)

asRaw :: forall a. (FiniteBits a) => a -> [Bool]
asRaw = asRawN (bits @a size)

asRawN :: (FiniteBits a) => Int -> a -> [Bool]
asRawN n a = map
    (testBit a)
    [n-1, n-2 .. 0]

fromRaw :: forall a. (FiniteBits a) => [Bool] -> a
fromRaw bs = getIor 
    . mconcat
    . zipWith (\pos shouldSet -> if shouldSet 
            then Ior (bit pos) 
            else mempty) 
        [truncatedSize-1, truncatedSize-2 .. 0]
    $ truncatedBits
    where 
        truncatedBits = drop (length bs - size') bs
        truncatedSize = length truncatedBits
        size' = bits @a size

convert :: forall a b. (FiniteBits a, FiniteBits b) => a -> b
convert a = fromRaw @b (asRaw a)

class Signeable a where
    asSigned :: a -> Int
    asUnsigned :: a -> Int

instance Signeable SicWord where
    asSigned :: SicWord -> Int
    asSigned a@(SicWord w)
        | w <= 2^(finiteBitSize a - 1) - 1 = fromIntegral w
        | otherwise     = fromIntegral $ - (complement w .&. 0x7FFFFF) - 1

    asUnsigned :: SicWord -> Int
    asUnsigned (SicWord a) = a

(<.) :: (Integral a, FiniteBits a, Integral b, FiniteBits b) => a -> b -> Word
(<.) a b = (fromIntegral a .<<. finiteBitSize b) + fromIntegral b

-- Property type --------------------------------------------

type Get a = (Machine -> a)
type Set a = (Machine -> a) -> Update
type Update = S.StateT Machine IO ()

-- Errors ---------------------------------------------------

data SicError =
    InvalidMnemonic     !String |
    InvalidAddressing   !String |
    InvalidRegister     !Word |
    InvalidOp           !Word

instance Exception SicError
instance Show SicError where
    show :: SicError -> String
    show (InvalidOp op) = printf "Invalid operand 0x%02X" op

-- Machine --------------------------------------------------

data Machine = Machine 
    { _a :: SicWord
    , _x :: SicWord
    , _l :: SicWord
    , _b :: SicWord
    , _s :: SicWord
    , _t :: SicWord
    , _f :: SicFloat
    , _pc :: SicWord
    , _sw :: Ordering
    , _memory :: Memory
    }
makeLenses ''Machine

instance Eq Machine where
  (==) :: Machine -> Machine -> Bool
  (==) a b = a^.pc == b^.pc

showMemory :: Address -> Address -> Machine -> String
showMemory start@(SicWord startRaw) end@(SicWord endRaw) state = let
    rowLength       = 16
    start'          = fromIntegral startRaw - offset
    end'            = fromIntegral endRaw
    offset          = fromIntegral startRaw `mod` 16
    startPadding    = replicate offset "  "

    rows            = chunksOf rowLength (startPadding ++ bytes)
    rowAddresses    = map (printf "0x%06X")             [start', start' + rowLength .. end'-1]
    bytes           = map (printf "%02X") memorySect
    memorySect      = map (\a -> state ^. byte a) [start..end-1]

    prependAddr     = zipWith (:) rowAddresses
    joinRow         = map unwords
    joinRows        = intercalate "\n"

    in (joinRows . joinRow . prependAddr $ rows)

initialize :: Machine
initialize =
    Machine 
        0 0 0 0 0 0 0 0 EQ              -- Registers
        empty                           -- Memory

register :: Word -> Lens' Machine SicWord
register num =
    case num of
        0 -> a
        1 -> x
        2 -> l
        3 -> b
        4 -> s
        5 -> t
        _ -> throw (InvalidRegister num)

registers :: [Word] -> [Lens' Machine SicWord]
registers = map register
    
byte :: Address -> Lens' Machine SicByte
byte addr = lens getter setter
    where
        getter :: Machine -> SicByte
        getter state = state ^. memory . at addr . to (fromMaybe 0x00)

        setter :: Machine -> SicByte -> Machine
        setter state b = state & memory . at addr ?~ b

memorized :: forall a. FiniteBits a => Address -> Lens' Machine a
memorized addr = lens getter setter
    where
        getter :: Machine -> a
        getter state = let
            bytesToFetch = map (\a -> state^.byte a) [addr .. addr+(fromIntegral n-1)]
            n = bytes @a size
            in fromRaw $ concatMap (asRaw @SicByte) bytesToFetch
                
        setter :: Machine -> a -> Machine
        setter state val = let

            bytesToStore = map
                (fromRaw @SicByte)
                (chunksOf 8 (asRaw @a val))

            updates = foldl1' (.) $ 
                zipWith (\a b -> byte a .~ b)
                    [addr .. ]
                    bytesToStore

            in updates state

data Addressing 
    = Immediate
    | Simple
    | Indirect
    deriving Eq

data Offset
    = Base
    | PC
    | Direct
    | Invalid
    deriving Eq

memoryCell :: (FiniteBits a) => Addressing -> Getter Machine Address -> Lens' Machine a
memoryCell addressing addr = lens getter setter
    where
        getter :: FiniteBits a => Machine -> a
        getter state = case addressing of
            Simple      -> state^.memorized (state^.addr)
            Immediate   -> convert (state^.addr)
            Indirect    -> state ^. memoryCell Simple (like (state ^. memoryCell @SicWord Simple addr))
        
        setter :: FiniteBits a => Machine -> a -> Machine
        setter state val = case addressing of
            Simple      -> state & memorized (state^.addr) .~ val
            Immediate   -> state & memoryCell Simple addr .~ val 
            Indirect    -> state & memoryCell Simple (like (state ^. memoryCell Simple addr)) .~ val
            
-----------------------------------------------------------