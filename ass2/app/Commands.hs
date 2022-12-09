{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Commands where
import Machine hiding (Update)
import Data.Int
import Control.Monad
import Data.Maybe
import Data.Functor
import Data.Bits
import Data.Word
import Data.Tuple
import Data.List
import Control.Exception
import Data.Map (Map, empty, lookup)
import qualified Data.Map as M
import Numeric
import GHC.Show
import Text.Printf
import qualified Control.Monad.State.Strict as S
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Pipes.Core
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Ge
import qualified Data.Binary.Put as Pu
import Control.Concurrent
import Data.IORef
import Control.Lens hiding ((<.))
import Data.Bits.Lens hiding (bits)

type OpCode     = Word
data Nixbpe     = Nixbpe Bool Bool Bool Bool Bool Bool

instance Show Nixbpe where
    show :: Nixbpe -> String
    show (Nixbpe n i x b p e) = concatMap (show . fromEnum) [n, i, x, b, p ,e]

asInt :: Nixbpe -> Int
asInt (Nixbpe n i x b p e) = fromRaw [n, i, x, b, p, e]

data Operand =
    OF3 Int | 
    OF4 Int | 
    OSIC Int

instance Signeable Operand where
    asSigned :: Operand -> Int
    asSigned o = let
        (int, size) = case o of
            OF3 a   -> (a, 12)
            OSIC a  -> (a, 15)
            OF4 a   -> (a, 20)

        lowerMask = getIor $ mconcat $ map (Ior . bit) [0..11]

        asSigned'
            | int <= 2^(size-1) - 1 = fromIntegral int
            | otherwise = - (complement int .&. lowerMask) - 1
        in asSigned'

    asUnsigned :: Operand -> Int
    asUnsigned o = case o of
        (OF3 a) -> a
        (OF4 a) -> a
        (OSIC a) -> a

isIndexed :: Nixbpe -> Bool
isIndexed (Nixbpe n i x b p e) = x

addressing :: Nixbpe -> Addressing
addressing (Nixbpe n i x b p e) = case (n, i) of
    (False, True)   -> Immediate
    (True, False)   -> Indirect
    (_, _)          -> Simple

isExtended :: Nixbpe -> Bool
isExtended (Nixbpe n i x b p e) = e

offset :: Nixbpe -> Offset
offset (Nixbpe n i x b p e) = case (b, p) of
    (True, False)   -> Base
    (False, True)   -> PC
    (False, False)  -> Direct
    _               -> Invalid

applyOffset :: Nixbpe -> Operand -> Getter Machine Int
applyOffset nixbpe addr = let
    addressing' = addressing nixbpe
    offset'     = offset nixbpe
    xOffset     = if isIndexed nixbpe
        then if addressing' == Simple 
            then x
            else throw (InvalidAddressing "indexing requires simple addressing")
        else like 0
    
    in case offset' of
    Base -> b . to ((+ asUnsigned addr) . asUnsigned)
    PC -> to $ do
        pc' <- view pc
        x   <- view xOffset
        return $ asUnsigned pc' + asSigned x + asSigned addr
    Direct -> like $ fromIntegral (asUnsigned addr)
    Invalid -> throw (InvalidAddressing "b and p both set to true")

data Request = Consume SicByte | Produce SicByte SicByte

type DeviceMap = M.Map SicByte Device
type Update = Client Request SicByte (S.StateT Machine IO) ()
type PureUpdate = S.StateT Machine IO ()

data Command =
    F1   String Word Update |
    F2   String Word (Word -> Word -> Update) |
    F34  String Word (Nixbpe -> Operand -> Update)

data ParsedCommand =
    F1' OpCode Update |
    F2' OpCode Word Word Update |
    F34' OpCode Nixbpe Operand Update

type Prop a = Lens' Machine a

showOp :: OpCode -> String
showOp = printf "0x%02X"

showReg :: Word -> String
showReg = printf "0x%1X"

showAddress :: Word -> String
showAddress = printf "0x%05X"

opCode :: Command -> OpCode
opCode (F1 _ o _) = o
opCode (F2 _ o _) = o
opCode (F34 _ o _) = o

name :: Command -> String
name (F1 n _ _) = n
name (F2 n _ _) = n
name (F34 n _ _) = n

parsedCommand :: ParsedCommand -> Update
parsedCommand (F1' _ u) = u
parsedCommand (F2' _ _ _ u) = u
parsedCommand (F34' _ _ _ u) = u

withRegisters :: ((Word, Prop SicWord) -> (Word, Prop SicWord) -> PureUpdate) -> Word -> Word -> Update
withRegisters f reg1 reg2 = noIO $ f (toRegister reg1) (toRegister reg2)

toRegister :: Word -> (Word, Prop SicWord)
toRegister word = (word, register word)

toOperand :: forall a. FiniteBits a => Nixbpe -> Operand -> Prop a
toOperand nixbpe addr = let
    offset'     = offset nixbpe
    addressing' = addressing nixbpe
    memoryProp' = memoryCell addressing'
    in if not (isExtended nixbpe)
        then memoryProp' (applyOffset nixbpe addr . to fromIntegral)
        else if offset' /= Direct 
            then throw (InvalidAddressing "base and pc addressing is unavailable in F4")
            else memoryProp' (like . fromIntegral . asUnsigned $ addr)

withOperandIO :: forall a. FiniteBits a => (Prop a -> Update) -> Nixbpe -> Operand -> Update
withOperandIO f nixbpe addr = f (toOperand nixbpe addr)

withOperand :: forall a. FiniteBits a => (Prop a -> PureUpdate) -> Nixbpe -> Operand -> Update
withOperand f nixbpe addr = noIO $ f (toOperand nixbpe addr)

toAddress :: Nixbpe -> Operand -> Getter Machine Int
toAddress nixbpe addr = let
    addressing' = case addressing nixbpe of
        Indirect    -> Indirect
        _           -> Immediate
    offset'     = offset nixbpe
    memoryGet' = memoryCell addressing'
    in if not (isExtended nixbpe)
        then memoryGet' (applyOffset nixbpe addr . to fromIntegral)
        else if offset' /= Direct
            then throw (InvalidAddressing "base and pc addressing is unavailable in F4")
            else memoryGet' (like . fromIntegral . asUnsigned $ addr)

withAddress :: (Getter Machine Int -> PureUpdate) -> Nixbpe -> Operand -> Update
withAddress f nixbpe addr = noIO $ f (toAddress nixbpe addr)

noIO :: PureUpdate -> Update
noIO f = do 
    lift f
    return ()

commands :: [Command]
commands = [

        F34 "ADD"       0x18 (withOperand @SicWord (use >=> (a +=))),

        F34 "ADDF"      0x58 (withOperand @SicFloat (use >=> (f .=))),
        
        F2  "ADDR"      0x90 (withRegisters (\(_, reg1) (_, reg2) -> use reg1 >>= (reg2 +=))),

        F34 "AND"       0x40 (withOperand @SicWord (use >=> (a .&.=))),

        F2  "CLEAR"     0xB4 (withRegisters (\(_, reg1) (_, _) -> reg1 .= 0)),
        
        F34 "COMP"      0x28 (withOperand @SicWord (\operand -> do
            a       <- use a
            word    <- use operand
            sw .= compare (asSigned a) (asSigned word))),
        
        F34 "COMPF"     0x88 (withOperand @SicFloat (\operand -> do
            f   <- use f
            op  <- use operand
            sw .= compare f op)),

        F2  "COMPR"     0xA0 (withRegisters (\(_, reg1) (_, reg2) -> do
            r1 <- use reg1
            r2 <- use reg2
            sw .= compare (asSigned r1) (asSigned r2))),

        F34 "DIV"       0x24 (withOperand @SicWord (\operand -> do
            a'  <- use a
            op  <- use operand
            a .= fromIntegral (asSigned a' `div` asSigned op))),
        
        F2  "DIVR"      0x9C (withRegisters (\(_, reg1) (_, reg2) -> do
            r1 <- use reg1
            r2 <- use reg2
            reg2 .= fromIntegral (asSigned r1 `div` asSigned r2))),
        
        F34 "DIVF"      0x64 (withOperand @SicFloat (use >=> (f //=))),

        F1  "FIX"       0xC4 (noIO $ use f >>= ((a .=) . floor)),

        F1  "FLOAT"     0xC0 (noIO $ use a >>= ((f .=) . fromIntegral . asSigned)),

        F34 "J"         0x3C (withAddress (use >=> ((pc .=) . fromIntegral))),
        
        F34 "JEQ"       0x30 (withAddress (\operand -> do
            pc' <- use pc
            cc' <- use sw
            op <- use operand
            pc .= if cc' == EQ
                then fromIntegral op
                else pc')),

        F34 "JGT"       0x34 (withAddress (\operand -> do
            pc' <- use pc
            cc' <- use sw
            op <- use operand
            pc .= if cc' == GT
                then fromIntegral op
                else pc')),
        
        F34 "JLT"       0x38 (withAddress (\operand -> do
            pc' <- use pc
            cc' <- use sw
            op <- use operand
            pc .= if cc' == LT
                then fromIntegral op
                else pc')),
        
        F34 "JSUB"      0x48 (withAddress (\operand ->
            use pc >>= (l .=) >>
            use operand >>= ((pc .=) . fromIntegral))),

        F34 "LDCH"      0x50 (withOperand @SicByte (\operand -> do
            byte    <- use operand
            a'      <- use a
            let lsb = fromIntegral byte
                msbs = a' .&. 0xFFFF00

            a .= msbs .|. lsb)),
        
        F34 "LDA"       0x00 (withOperand @SicWord (use >=> (a .=))),
        
        F34 "LDB"       0x68 (withOperand @SicWord (use >=> (b .=))),
        
        F34 "LDF"       0x70 (withOperand @SicFloat (use >=> (f .=))),
        
        F34 "LDL"       0x08 (withOperand @SicWord (use >=> (l .=))),
        
        F34 "LDS"       0x6C (withOperand @SicWord (use >=> (s .=))),
        
        F34 "LDT"       0x74 (withOperand @SicWord (use >=> (t .=))),
        
        F34 "LDX"       0x04 (withOperand @SicWord (use >=> (x .=))),

        F34 "MUL"       0x20 (withOperand @SicWord (\operand -> do
            a'  <- use a
            op  <- use operand
            a .= fromIntegral (asSigned a' * asSigned op))),
        
        F34 "MULF"      0x60 (withOperand @SicFloat (use >=> (f *=))),

        F2  "MULR"      0x98 (withRegisters (\(_, reg1) (_, reg2) -> use reg1 >>= (reg2 *=))),
        
        F34 "OR"        0x44 (withOperand @SicWord (use >=> (a .|.=))),

        F2  "RMO"       0xAC (withRegisters (\(_, reg1) (_, reg2) -> use reg1 >>= (reg2 .=))),
        
        F34 "RSUB"      0x4C (withOperand @SicWord (\operand -> use l >>= (pc .=))),
        
        F2  "SHIFTL"    0xA4 (withRegisters (\(_, reg1) (n,_) -> do
            let n'  = fromIntegral n + 1
                s   = bits (size @SicWord)
            r       <- use reg1
            reg1 .= r .<<. n' .|. r .>>. (s - n'))),
        
        F2  "SHIFTR"    0xA8 (withRegisters (\(_, reg1) (n,_) -> do
            let n'  = fromIntegral n + 1
                s   = bits (size @SicWord)
            r       <- use reg1
            reg1 .= r .>>. n')),
        
        F34 "STCH"      0x54 (withOperand @SicByte (\operand ->
            use a >>= ((operand .=) . fromIntegral . (.&. 0xFF)))),

        F34 "STA"       0x0C (withOperand (\operand ->
            use a >>= (operand .=))),
        
        F34 "STB"       0x78 (withOperand (\operand ->
            use b >>= (operand .=))),
        
        F34 "STF"       0x80 (withOperand (\operand ->
            use f >>= (operand .=))),
        
        F34 "STL"       0x14 (withOperand (\operand ->
            use l >>= (operand .=))),
        
        F34 "STS"       0x7C (withOperand (\operand ->
            use s >>= (operand .=))),
        
        F34 "STT"       0x84 (withOperand (\operand ->
            use t >>= (operand .=))),
        
        F34 "STX"       0x10 (withOperand (\operand ->
            use x >>= (operand .=))),

        F34 "SUB"       0x1C (withOperand @SicWord (use >=> (a -=))),

        F34 "SUBF"      0x5C (withOperand @SicFloat (use >=> (f -=))),
        
        F2  "SUBR"      0x94 (withRegisters (\(_, reg1) (_, reg2) -> use reg1 >>= (reg2 -=))),
        
        F34 "TIX"       0x2C (withOperand (\operand -> do 
            x += 1
            x' <- use x
            op <- use operand
            sw .= compare x' op)),
        
        F2  "TIXR"      0xB8 (withRegisters (\(_, reg1) (_, _) -> do
            x += 1
            x' <- use x
            r1 <- use reg1
            sw .= compare x' r1)),

        F34 "RD"        0xD8 (withOperandIO @SicByte (\operand -> do
            device <- use operand
            byte <- request $ Consume device
            lift $ a .= convert byte)),

        F34 "WD"        0xDC (withOperandIO @SicByte (\operand -> do
            device  <- use operand
            a'      <- use a
            byte    <- request $ Produce device (convert a')
            lift $ a .= convert byte))
        
    ]

data Device = 
    InDevice (IO SicByte) |
    OutDevice (SicByte -> IO ()) |
    FileDevice Handle

fileDevice :: SicByte -> IO Device
fileDevice num = do
    let path = printf "%02X.dev" num
    handle <- openBinaryFile path ReadWriteMode
    hSetBuffering handle NoBuffering
    return $ FileDevice handle

deviceServer :: IORef DeviceMap -> Request -> IO SicByte
deviceServer mdevices req  = do
        
    devices <- readIORef mdevices
    let (name, byte) = case req of
            Consume name -> (name, Nothing) 
            Produce name byte -> (name, Just byte)

        exists = M.member name devices

    device <- if exists 
        then do
            return (devices M.! name)
        else do 
            fileDevice name
    
    byte' <- case byte of
        Nothing -> do
            case device of
                InDevice producer -> do producer
                
                FileDevice handle -> do
                    string <- BS.hGet handle 1
                    let byte = if BS.null string 
                        then 0x00
                        else Ge.runGet Ge.getInt8 (BS.fromStrict string)
                    return . fromIntegral . fromEnum $ byte
        
        Just b -> do
            case device of 
                OutDevice consumer -> consumer b >> return b
                FileDevice handle -> 
                    BS.hPut handle (BL.toStrict $ Pu.runPut (Pu.putWord8 (convert b)))
                    >> return b

    unless exists $ do
        writeIORef mdevices (M.insert name device devices)
        return ()

    return byte'

commandMap :: Map OpCode Command
commandMap = 
    M.fromList $ zip (map opCode commands) commands

parseCommand :: Get ParsedCommand
parseCommand = do
    pc'     <- view pc
    fetch0 <- view $ byte (pc'+0)
    fetch1 <- view $ byte (pc'+1)
    fetch2 <- view $ byte (pc'+2)
    fetch3 <- view $ byte (pc'+3)

    let op      = fromIntegral $ fetch0 .&. 0xFC
        r1      = fromIntegral $ fetch1 .>>. 4
        r2      = fromIntegral $ fetch1 .&. 0x0F
        sAddr   = (fetch1 <. fetch2) .&. 0x7FFF
        f3Addr  = sAddr .&. 0xFFF
        f4Addr  = f3Addr <. fetch3

    let [n, i, x, b, p, e]  = asRawN 6 (((fetch0 <. fetch1) .>>. 4) .&. 0x3F)
        nixbpe              = Nixbpe n i x b p e

    let operation = case M.lookup op commandMap of
            Just command    -> command
            Nothing         -> throw (InvalidOp op)

    return $ case operation of
        F1 _ _ updater    -> F1' op         (noIO (pc += 1) >> updater)
        F2 _ _ updater    -> F2' op r1 r2   (noIO (pc += 2) >> updater r1 r2)
        F34 _ _ updater   -> if n || i 
            then if e
                then 
                    let operand = (OF4 . fromIntegral $ f4Addr)
                    in F34' op nixbpe operand (noIO (pc += 4) >> updater nixbpe operand)
                else 
                    let operand = (OF3 . fromIntegral $ f3Addr)
                    in F34' op nixbpe operand (noIO (pc += 3) >> updater nixbpe operand)
            else 
                let operand = (OSIC . fromIntegral $ sAddr)
                in F34' op nixbpe operand (noIO (pc += 3) >> updater nixbpe operand)
