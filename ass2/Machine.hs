{-# LANGUAGE TypeFamilies #-}

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
import Data.Typeable
import Text.Printf
import GHC.Prim
import Data.Maybe

-- Type definitions -----------------------------------------

maxAddress :: Word32
maxAddress = 2^20-1

type SicByte    = Word8
type SicWord    = Word32
type SicFloat   = Double
type Address    = Word32
type RegI       = SicByte
type Memory     = Map Word32 SicByte

-- Registers ------------------------------------------------

data Size a = Bytes Int | Bits Int

bitsSize :: Size a -> Int
bitsSize (Bytes a) = a * 8
bitsSice (Bits a) = a

byteSize :: Size a -> Int
byteSize (Bytes a) = a
byteSize (Bits a) = ceiling (fromIntegral a / 8.0)

class (Num (Underlying b), FiniteBits (Underlying b)) => Storage b where
    type Underlying b

    size :: Size b
    toUnderlying :: b -> Underlying b
    fromUnderlying :: Underlying b -> b

    toBits :: b -> [Bool]
    toBits = intToBits (bitsSize (size @b)) . toUnderlying

    fromBits :: [Bool] -> b
    fromBits = fromUnderlying . bitsToInt (bitsSize (size @b))

instance Storage SicWord where
    type Underlying SicWord = SicWord
    size            = Bytes 3
    toUnderlying a
        | a >= 0    = a .&. 0xFFFFFF
        | otherwise = complement (-a - 1) .&. 0xFFFFFF
    fromUnderlying a
        | a <= 2^24 - 1 = a .&. 0xFFFFFF
        | otherwise     = - (complement a - 1) .&. 0xFFFFFF

instance Storage SicFloat where
    type Underlying SicFloat = Word64
    size            = Bytes 6
    toUnderlying    = fromIntegral          . (.>>. 16) . castDoubleToWord64
    fromUnderlying  = castWord64ToDouble    . (.<<. 16) . fromIntegral

instance Storage Ordering where
    type Underlying Ordering = SicWord
    size            = Bytes 3
    toUnderlying    = fromIntegral . fromEnum
    fromUnderlying  = toEnum . fromIntegral

instance Storage SicByte where
    type Underlying SicByte = SicByte
    size            = Bytes 1
    toUnderlying    = id
    fromUnderlying  = id

class Signeable a where
    toSigned :: a -> Int
    fromSigned :: Int -> a

instance Signeable SicWord where
    toSigned :: SicWord -> Int
    toSigned a
        | a <= 2^23 - 1 = fromIntegral a
        | otherwise     = fromIntegral $ - (complement a .&. 0x7FFFFF) - 1

    fromSigned :: Int -> SicWord
    fromSigned = toUnderlying @SicWord . fromIntegral

bitsToInt :: (Num a, Bits a) => Int -> [Bool] -> a
bitsToInt size bs = foldl1'
    (\acc b -> acc*2 + b)
    (map (fromIntegral . fromEnum) (take size bs))

intToBits :: (Bits a) => Int -> a -> [Bool]
intToBits size a = map
    (testBit a)
    [size-1, size-2 .. 0]

(<.) :: (Integral a, FiniteBits a, Integral b, FiniteBits b) => a -> b -> Word
(<.) a b = ((fromIntegral a) .<<. (finiteBitSize b)) + fromIntegral b

-- Property type --------------------------------------------

data Get a = Get (Machine -> a)
data Set a = Set (Get a -> Get Machine)
data Prop b = Prop {
    get :: Get b,
    set :: Set b
}


instance Semigroup (Get Machine) where
    (<>) (Get a) (Get b) = Get (a . b)

(<!>) :: (Semigroup a) => a -> a -> a
(<!>) a b = b <> a

instance Monoid (Get Machine) where
    mempty = pure initialize


instance Functor Get where
    fmap f (Get a) = Get (\state -> f (a state))

instance Applicative Get where
    pure a = Get (\state -> a)
    (<*>) (Get f) (Get g) = Get (\state -> (f state) (g state))

instance Monad Get where
    (>>=) (Get m) f = Get (\state -> _get (f (m state)) state)
        where _get (Get a) = a


(=.) :: Prop a -> Get a -> Get Machine
(=.) (Prop _ (Set setter)) = setter

(@=) :: (Storage a) => Get Address -> Get a -> Get Machine
(@=) addr val = do
    addr' <- addr
    store addr' =@ val

(@) :: Get a -> Machine -> a
(@) (Get a) state = a state

(=@) :: Set a -> Get a -> Get Machine
(=@) (Set a) val = a val

-- Errors ---------------------------------------------------

instance Exception SicError
data SicError =
    InvalidMnemonic     !String |
    InvalidAddressing   !String |
    InvalidRegister     !SicByte |
    InvalidOp           !SicByte

instance Show SicError where
    show (InvalidOp op) = printf "Invalid operand 0x%02X" op

-- Machine --------------------------------------------------

data Machine = Machine 
    { a :: SicWord
    , x :: SicWord
    , l :: SicWord
    , b :: SicWord
    , s :: SicWord
    , t :: SicWord
    , f :: SicFloat
    , pc :: SicWord
    , sw :: Ordering
    , memory :: Memory
    }

instance Show Machine where
    show m = printf
        ("a  = %024b %8d 0x%06X\n" ++ 
        "x  = %024b %8d 0x%06X\n" ++ 
        "l  = %024b %8d 0x%06X\n" ++ 
        "s  = %024b %8d 0x%06X\n" ++ 
        "t  = %024b %8d 0x%06X\n" ++ 
        "b  = %024b %8d 0x%06X\n" ++ 
        "pc = %024b %8d 0x%06X\n" ++ 
        "sw = %024b %8s\n" ++
        "f  = %048b %f")
        (toUnderlying (a m)) (a m) (a m)
        (toUnderlying (x m)) (x m) (x m)
        (toUnderlying (l m)) (l m) (l m)
        (toUnderlying (s m)) (s m) (s m)
        (toUnderlying (t m)) (t m) (t m)
        (toUnderlying (b m)) (b m) (b m)
        (toUnderlying (pc m)) (pc m) (pc m)
        (toUnderlying (sw m)) (show (sw m))
        (toUnderlying (f m)) (f m)

showMemory :: Address -> Address -> Machine -> String
showMemory start end state = let
    rowLength       = 16
    start'          = fromIntegral start - offset
    end'            = fromIntegral end
    offset          = fromIntegral start `mod` 16
    startPadding    = replicate offset "  "

    rows            = (chunksOf rowLength (startPadding ++ bytes))
    rowAddresses    = map (printf "0x%06X")             [start', start' + rowLength .. end'-1]
    bytes           = map (printf "%02X" . fromMaybe 0) memorySect
    memorySect      = map (`M.lookup` (memory state))   [start..end-1]

    prependAddr     = zipWith (:) rowAddresses
    joinRow         = map (intercalate " ")
    joinRows        = intercalate "\n"

    in (joinRows . joinRow . prependAddr $ rows)

a' (Get new)    = Get (\state -> state { a = new state })
x' (Get new)    = Get (\state -> state { x = new state })
l' (Get new)    = Get (\state -> state { l = new state })
b' (Get new)    = Get (\state -> state { b = new state })
s' (Get new)    = Get (\state -> state { s = new state })
t' (Get new)    = Get (\state -> state { t = new state })
pc' (Get new)   = Get (\state -> state { pc = new state })
sw' (Get new)   = Get (\state -> state { sw = new state })
f' (Get new)    = Get (\state -> state { f = new state })

regA    = Prop (Get a) (Set a')
regX    = Prop (Get x) (Set x')
regL    = Prop (Get l) (Set l')
regB    = Prop (Get b) (Set b')
regS    = Prop (Get s) (Set s')
regT    = Prop (Get t) (Set t')
regPC   = Prop (Get pc) (Set pc')
regSW   = Prop (Get sw) (Set sw')
regF    = Prop (Get f) (Set f')

initialize :: Machine
initialize = Machine (0) (0) (0) (0) (0) (0) (0) (0) (EQ) empty

register :: SicByte -> Prop SicWord
register num =
    case num of
        0 -> regA
        1 -> regX
        2 -> regL
        3 -> regB
        4 -> regS
        5 -> regT
        _ -> throw (InvalidRegister num)

registers :: [SicByte] -> [Prop SicWord]
registers = map (register)
    
fetchRaw :: Address -> Machine -> SicByte
fetchRaw addr state = 
    case M.lookup addr (memory state) of
        Just byte   -> byte
        Nothing     -> 0x00

fetchRawN :: Address -> Int -> Machine -> [SicByte]
fetchRawN addr n state =
    map 
        (\a -> fetchRaw a state)
        [addr .. addr+(fromIntegral n)]

storeRaw :: Address -> SicByte -> Machine -> Machine
storeRaw addr byte state =
    state { 
        memory = M.insert addr byte (memory state)
    }

storeRawN :: Address -> [SicByte] -> Machine -> Machine
storeRawN addr bs =
    foldl1' (.) $
        map
        (\(i, b) -> 
            storeRaw (addr+i) b)
        (zip [0..] bs)

fetch :: forall a. Storage a => Address -> Get a
fetch addr = do 
    let size'   = byteSize (size @a)
    bytes       <- Get $ fetchRawN addr size'
    let bits    = concatMap (toBits @SicByte) bytes
            
    return $ fromBits bits

store :: forall a. (Storage a) => Address -> Set a
store addr = Set (\val -> do
    val' <- val
    state' <- Get $ storeRawN addr $ map
        (fromBits @SicByte)
        (chunksOf 8 (toBits @a val'))
    return state')

data Addressing = Immediate | Simple | Indirect deriving Eq
data Offset = Base | PC | Direct | Invalid deriving Eq

memoryCell :: (Storage a) => Addressing -> Get Address -> Prop a
memoryCell addressing addr = 
    case addressing of
        Simple -> Prop
            (addr >>= fetch)
            (Set (addr @=))
        
        Immediate -> Prop
            (fromUnderlying . fromIntegral <$> addr)
            (Set (pure (Get id)))
        
        Indirect -> Prop
            (get $ memoryCell 
                Simple 
                (get $ memoryCell @SicWord 
                    Simple
                    addr))
            (Set (get (memoryCell @SicWord Simple addr) @=))

-------------------------------------------------------------