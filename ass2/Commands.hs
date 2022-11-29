module Commands where
import Machine
import Data.Int
import Control.Monad
import Data.Functor
import Data.Bits
import Data.Word
import Data.Tuple
import Data.List
import Control.Exception
import Data.Map (Map, empty, lookup)
import qualified Data.Map as M

type OpCode     = SicByte
data Nixbpe     = Nixbpe Bool Bool Bool Bool Bool Bool

data Operand = OF3 Word32 | OF4 Word32 | OSIC Word32

toIntegral :: (Bits a, Integral a) => Operand -> a
toIntegral (OF3 a) = fromIntegral a
toIntegral (OSIC a) = fromIntegral a
toIntegral (OF4 a) = fromIntegral a

bitCount :: Operand -> Int
bitCount (OF3 _) = 12
bitCount (OSIC _) = 15
bitCount (OF4 _) = 20

bitCount' :: Operand -> Int
bitCount' (OF3 _) = 16
bitCount' (OSIC _) = 16
bitCOunt' (OF4 _) = 32

-- unwrap :: (Bits a, Integral a) => Operand -> a
unwrap (OF3 a) = a
unwrap (OSIC a) = a
unwrap (OF4 a) = a

lowerMask :: Operand -> Word32
lowerMask a = (foldl1' (.) $ map (\n -> (`clearBit` n)) [0..bitCount a-1]) (unwrap a)

upperSign :: Operand -> Word32
upperSign a = (foldl1' (.) $ map (\n -> (`setBit` n) . (`clearBit` n)) [bitCount a..bitCount' a]) (unwrap a)
    
unsigned :: Operand -> Word32
unsigned = lowerMask

signed :: Operand -> Int
signed = fromIntegral . upperSign

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

applyOffset :: Nixbpe -> Operand -> Get Int
applyOffset nixbpe addr = let
    addressing' = addressing nixbpe
    offset'     = offset nixbpe
    xOffset     = if isIndexed nixbpe
        then if addressing' == Simple 
            then get regX
            else throw (InvalidAddressing "indexing requires simple addressing")
        else pure 0
    
    in case offset' of
    Base -> do
        b <- get regB
        return . fromIntegral $ b + (unsigned addr)
    PC -> do
        pc  <- get regPC
        x   <- xOffset
        let accOffset   = fromIntegral pc + (toSigned x)
            addr'       = if -accOffset < (fromIntegral . unsigned) addr
                then (fromIntegral . unsigned) addr + abs accOffset
                else throw (InvalidAddressing "Offset caused negative address")
        return $ addr'
    Direct -> pure (fromIntegral (unsigned addr))
    Invalid -> throw (InvalidAddressing "b and p both set to true")


type StateUpdate = Machine -> Machine
data Command =
    F1   String SicByte (Get Machine) |
    F2   String SicByte (SicByte -> SicByte -> Get Machine) |
    F34  String SicByte (Nixbpe -> Operand -> Get Machine)

op :: Command -> OpCode
op (F1 _ o _) = o
op (F2 _ o _) = o
op (F34 _ o _) = o

(&.) :: StateUpdate -> StateUpdate -> StateUpdate
(&.) a b =  b . a

withRegisters :: ((SicByte, Prop SicWord) -> (SicByte, Prop SicWord) -> Get Machine) -> SicByte -> SicByte -> Get Machine
withRegisters f reg1 reg2 =
    let [r1, r2] = registers [reg1, reg2]
    in f (reg1, r1) (reg2, r2)

withOperand :: forall a. Storage a => (Prop a -> Get Machine) -> Nixbpe -> Operand -> Get Machine
withOperand f nixbpe addr = f $ let
    offset'     = offset nixbpe
    addressing' = addressing nixbpe
    memoryProp' = memoryCell addressing'
    xOffset     = if isIndexed nixbpe
        then if addressing' == Simple 
            then get regX
            else throw (InvalidAddressing "indexing requires simple addressing")
        else pure 0
    in if not (isExtended nixbpe)
    then memoryProp' (fromIntegral <$> (applyOffset nixbpe addr))
    else if offset' /= Direct 
        then throw (InvalidAddressing "base and pc addressing is unavailable in F4")
        else memoryProp' (pure (unsigned addr))
        
withAddress :: (Prop Address -> Get Machine) -> Nixbpe -> Operand -> Get Machine
withAddress f nixbpe addr = f $ let
    addressing' = case addressing nixbpe of
        Indirect    -> Indirect
        _           -> Immediate
    offset'     = offset nixbpe
    memoryProp' = memoryCell addressing'
    in if not (isExtended nixbpe)
    then memoryProp' (fromIntegral <$> (applyOffset nixbpe addr))
    else if offset' /= Direct
        then throw (InvalidAddressing "base and pc addressing is unavailable in F4")
        else memoryProp' (pure (unsigned addr))

commands :: [Command]
commands = [

        F34 "ADD"       0x18 (withOperand (\operand ->
            regA =. do
                op  <- get operand
                a   <- get regA
                return $ a + op)),
        
        F34 "ADDF"      0x58 (withOperand (\operand ->
            regF =. get operand)),
        
        F2  "ADDR"      0x90 (withRegisters (\(_, reg1) (_, reg2) ->
            reg2 =. do
            r1 <- get reg1
            r2 <- get reg2
            return $ r2 + r1)),

        F34 "AND"       0x40 (withOperand (\operand ->
            regA =. do
                a       <- get regA
                word    <- get operand
                return $ a .&. word)),

        F2  "CLEAR"     0xB4 (withRegisters (\(_, reg1) (_, _) ->
            reg1 =. pure 0)),
        
        F34 "COMP"      0x28 (withOperand @SicWord (\operand ->
            regSW =. do
                a       <- get regA
                word    <- get operand
                return $ compare (toSigned a) (toSigned word))),
        
        F34 "COMPF"     0x88 (withOperand @SicFloat (\operand ->
            regSW =. do
                f   <- get regF
                op  <- get operand
                return $ compare f op)),

        F2  "COMPR"     0xA0 (withRegisters (\(_, reg1) (_, reg2) ->
            regSW =. do
                r1 <- get reg1
                r2 <- get reg2
                return $ compare (toSigned r1) (toSigned r2))),

        F34 "DIV"       0x24 (withOperand @SicWord (\operand ->
            regA =. do
                a   <- get regA
                op  <- get operand
                return $ fromSigned ((toSigned a) `div` (toSigned op)))),
        
        F2  "DIVR"      0x9C (withRegisters (\(_, reg1) (_, reg2) ->
            reg2 =. do
                r1 <- get reg1
                r2 <- get reg2
                return $ fromSigned ((toSigned r1) `div` (toSigned r2)))),
        
        F34 "DIVF"      0x64 (withOperand @SicFloat (\operand ->
            regF =. do
                f   <- get regF
                op  <- get operand
                return $ f / op)),

        F2 "DIVF"       0x64 (withRegisters (\(_, reg1) (_, reg2) ->
            reg1 =. do
                r1   <- get reg1
                r2  <- get reg2
                return $ r2 `div` r1)),

        F1  "FIX"       0xC4 (regA =. (fmap floor (get regF))),

        F1  "FLOAT"     0xC0 (regF =. (fmap (fromIntegral . toSigned) (get regA))),

        F34 "J"         0x3C (withAddress (\operand ->
            regPC =. get operand)),
        
        F34 "JEQ"       0x30 (withAddress (\operand ->
            regPC =. do
                pc <- get regPC
                cc <- get regSW
                op <- get operand
                return $ if cc == EQ
                    then op
                    else pc)),

        F34 "JGT"       0x34 (withAddress (\operand ->
            regPC =. do
                pc <- get regPC
                cc <- get regSW
                op <- get operand
                return $ if cc == GT
                    then op
                    else pc)),
        
        F34 "JLT"       0x38 (withAddress (\operand ->
            regPC =. do
                pc <- get regPC
                cc <- get regSW
                op <- get operand
                return $ if cc == LT
                    then op
                    else pc)),
        
        F34 "JSUB"      0x48 (withAddress (\operand ->
            (regPC =. get operand) <>
            (regL =. get regPC))),
        
        F34 "LDA"       0x00 (withOperand (\operand ->
            regA =. get operand)),
        
        F34 "LDB"       0x68 (withOperand (\operand ->
            regB =. get operand)),
        
        F34 "LDCH"      0x50 (withOperand @SicByte (\operand ->
            regA =. do
                byte    <- get operand
                a       <- get regA
                let lsb = fromIntegral byte
                return $ (a .&. 0xFFFF00) .|. lsb)),
        
        F34 "LDF"       0x70 (withOperand (\operand ->
            regF =. get operand)),
        
        F34 "LDL"       0x08 (withOperand (\operand ->
            regL =. get operand)),
        
        F34 "LDS"       0x6C (withOperand (\operand ->
            regS =. get operand)),
        
        F34 "LDT"       0x74 (withOperand (\operand ->
            regT =. get operand)),
        
        F34 "LDX"       0x04 (withOperand (\operand ->
            regX =. get operand)),

        F34 "MUL"       0x20 (withOperand (\operand ->
            regA =. do
                a   <- get regA
                op  <- get operand
                return $ a * op)),
        
        F34 "MULF"      0x60 (withOperand (\operand ->
            regF =. do
                f   <- get regF
                op  <- get operand
                return $ f * op)),

        F2  "MULR"      0x98 (withRegisters (\(_, reg1) (_, reg2) ->
            reg2 =. do
                r1 <- get reg1
                r2 <- get reg2
                return $ r2 * r1)),
        
        F34 "OR"        0x44 (withOperand (\operand ->
            regA =. do
                a   <- get regA
                op  <- get operand
                return $ a .|. op)),

        F2  "RMO"       0xAC (withRegisters (\(_, reg1) (_, reg2) ->
            reg2 =. get reg1)),
        
        F34 "RSUB"      0x4C (withOperand @SicWord (\_ ->
            regPC =. get regL)),
        
        F2  "SHIFTL"    0xA4 (withRegisters (\(_, reg1) (n,_) ->
            reg1 =. do 
                let n'  = fromIntegral n + 1
                let s   = bitsSize (size @SicWord)
                r       <- get reg1
                return $ 
                    r .<<. (n') .|.
                    r .>>. (s - n'))),
        
        F2  "SHIFTR"    0xA8 (withRegisters (\(_, reg1) (n,_) ->
            reg1 =. do 
                let n'  = fromIntegral n + 1
                let s   = bitsSize (size @SicWord)
                r       <- get reg1
                return $ r .>>. n')),
        
        F34 "STA"       0x0C (withOperand (\operand ->
            operand =. get regA)),
        
        F34 "STB"       0x78 (withOperand (\operand ->
            operand =. get regB)),
        
        F34 "STCH"      0x54 (withOperand @SicByte (\operand ->
            operand =. do
                a       <- get regA
                return $ fromIntegral (a .&. 0xFF))),
        
        F34 "STF"       0x80 (withOperand (\operand ->
            operand =. get regF)),
        
        F34 "STL"       0x14 (withOperand (\operand ->
            operand =. get regL)),
        
        F34 "STS"       0x7C (withOperand (\operand ->
            operand =. get regS)),
        
        F34 "STT"       0x84 (withOperand (\operand ->
            operand =. get regT)),
        
        F34 "STX"       0x10 (withOperand (\operand ->
            operand =. get regX)),

        F34 "SUB"       0x1C (withOperand (\operand ->
            regA =. do
                a   <- get regA
                op  <- get operand
                return $ a - op)),

        F34 "SUBF"      0x5C (withOperand (\operand ->
            regF =. do
                f   <- get regF
                op  <- get operand
                return $ f - op)),
        
        F2  "SUBR"      0x94 (withRegisters (\(_, reg1) (_, reg2) ->
            reg2 =. do
                r1 <- get reg1
                r2 <- get reg2
                return $ r2 - r1)),
        
        F34 "TIX"       0x2C (withOperand (\operand ->
            (regX   =. ((+1)        <$> get regX)) <!>
            (regSW  =. ((compare)   <$> get regX <*> get operand)))),
        
        F2  "TIXR"      0xB8 (withRegisters (\(_, reg1) (_, _) ->
            (regX   =. ((+1)        <$> get regX)) <!>
            (regSW  =. ((compare)   <$> get regX <*> get reg1))))
        
    ]

commandMap :: Map OpCode Command
commandMap = 
    M.fromList $ zip (map op commands) commands

parseCommand :: Machine -> Get Machine
parseCommand s = do
    pc          <- get regPC
    fetch0      <- fetch @SicByte (pc+0)
    fetch1      <- fetch @SicByte (pc+1)
    fetch2      <- fetch @SicByte (pc+2)
    fetch3      <- fetch @SicByte (pc+3)

    let op      = fetch0 .&. 0xFC
        r1      = fetch1 .>>. 4
        r2      = fetch1 .&. 0x0F
        sAddr   = (fetch1 <. fetch2) .&. 0x7FFF
        f3Addr  = sAddr .&. 0xFFF
        f4Addr  = f3Addr <. fetch3

    let [n, i, x, b, p, e]  = intToBits 6 (((fetch0 <. fetch1) .>>. 4) .&. 0x3F)
        nixbpe              = Nixbpe n i x b p e

    let (f34Addr, f34PC) = if n || i
        then if e
            then (OF4 . fromIntegral $ f4Addr, pc+4)
            else (OF3 . fromIntegral $ f3Addr, pc+3)
        else (OSIC . fromIntegral $ sAddr, pc+3)

    let operation = case M.lookup op commandMap of
            Just command    -> command
            Nothing         -> throw (InvalidOp op)

    case operation of
        (F1 _ _ updater)    -> updater                  <> (regPC =. pure (pc+1))
        (F2 _ _ updater)    -> updater r1 r2            <> (regPC =. pure (pc+2))
        (F34 _ _ updater)   -> updater nixbpe f34Addr   <> (regPC =. pure  f34PC)