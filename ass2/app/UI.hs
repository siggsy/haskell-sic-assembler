{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module UI 
    ( drawInterface
    , handleEvent
    , chooseCursor
    , startEvent
    , attributeMap
    , initialState
    )
where

import Machine hiding (get)
import Commands
import Simulator

import Brick
import Graphics.Vty
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style
import Brick.Widgets.Table
import Brick.Widgets.Core
import Control.Concurrent

import Text.Printf
import Text.Wrap
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Brick.Keybindings as K
import Data.Text hiding (center, map, splitAt, replicate, take, length, toUpper, foldr, zip, unlines, foldr1)
import Data.Bits
import Data.Maybe
import Numeric
import Data.Char
import Control.Monad.State.Strict hiding (state)
import Brick.BChan
import Data.Foldable
import Brick.Widgets.Edit
import Data.Text.Zipper
import Control.Monad.Reader

import Control.Lens hiding (zoom)

data Name = Editor 
    deriving (Ord, Eq, Show)

data SimEvent = New (S.Seq Machine) (S.Seq SicByte)

data InputMode
    = Normal
    | Console
    deriving (Eq, Show)

data SimState = SimState
    { _console          :: Editor String Name
    , _mode             :: InputMode

    , _stdinChannel     :: BChan SicByte
    , _stdoutBuffer     :: Queue SicByte
    , _simulator        :: Simulator
    , _machineHistory   :: S.Seq Machine
    , _memoryStart      :: Address

    , _eventChan        :: BChan SimEvent
    }
makeLenses ''SimState

initialState :: Machine -> BChan SimEvent -> IO SimState
initialState state eventChan = do
    stdin <- newBChan 100
    stdout <- newQueue 100

    let deviceMap = M.fromList 
            [ (0x00, InDevice (readBChan stdin))
            , (0x01, OutDevice (atomically . writeQueue stdout))
            ]
    simulator <- newSimulator state deviceMap 10 50
    machineHistory <- peekQueue (history simulator)

    return $ SimState
        (editor Editor Nothing "")
        Normal

        stdin
        stdout
        simulator
        machineHistory
        0x00

        eventChan

attributeMap :: AttrMap
attributeMap = attrMap (fg white)
    [ (attrName "register",                 fg brightBlack)
    , (attrName "registerSet",  withStyle   (fg yellow) bold)
    , (attrName "registerName", withStyle   (fg yellow) bold)
    , (attrName "value",        withStyle   (fg yellow) bold)
    , (attrName "header",                   fg brightBlack)
    , (attrName "headerSet",    withStyle   (fg magenta) bold)
    , (attrName "border",                   fg brightBlack)
    ]

drawInterface :: SimState -> Widget Name
drawInterface s = 
    ((history <+> registers) <=> consoleBox) <+> memory
    where
        (machine S.:<| _) = s^.machineHistory

        registers = borderWithLabel (str "Registers") (
            vCenter 
            (drawRegisters machine))

        memory = borderWithLabel (str "Memory") (
            center 
            (drawMemory (s^.memoryStart) machine))

        consoleStyle = if s^.mode == Console
            then (overrideAttr borderAttr (attrName "registerSet"), withAttr (attrName "registerSet"))
            else (id, id)
        consoleBox =
                fst consoleStyle (
                borderWithLabel (snd consoleStyle $ str "Console") (
                    vCenter
                    (renderEditor (str . unlines) True (s^.console))))

        history = borderWithLabel (str "History") (
            center
            (drawCommandHistory (s^.machineHistory)))
    

drawCommandHistory :: S.Seq Machine -> Widget n
drawCommandHistory s = if S.length s == 0 
    then str "¯\\_(ツ)_/¯"
    else 
    renderTable
    . surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignLeft
    . setDefaultRowAlignment AlignBottom
    . table
    $ toList (fmap (commandEntry . parseCommand) s)

commandEntry :: ParsedCommand -> [Widget n]
commandEntry (F1' op _) = let
    definition = fromJust $ M.lookup op commandMap
    in [ str "F1", str $ name definition, emptyWidget ]

commandEntry (F2' op r1 r2 _) = let
    definition = fromJust $ M.lookup op commandMap
    in 
        [ withAttr (attrName "headerSet") (str "F2")
        , padLeft (Pad 1) (str $ name definition)
        , hBox . map (padLeft (Pad 1)) $
            [ hex (attrName "register") (attrName "registerSet") 2 2 (fromIntegral r1)
            , hex (attrName "register") (attrName "registerSet") 2 2 (fromIntegral r2)
            ]
        ]

commandEntry (F34' op nixbpe addr _) = let
    definition = fromJust $ M.lookup op commandMap
    in
        [ withAttr (attrName "headerSet") (str "F34")
        , padLeft (Pad 1) (str $ name definition)
        , hBox . map (padLeft (Pad 1)) $
            [ bin (attrName "register") (attrName "registerSet") 6 6 (asInt nixbpe)
            , str "0x" <+> hex (attrName "register") (attrName "registerSet") 6 6 (asUnsigned addr)
            , withAttr (attrName "value") . str $ printf "%d" (asUnsigned addr)
            , withAttr (attrName "value") . str $ printf "%d" (asSigned addr)
            ]
        ]


tableHeader :: [Widget n] -> [Widget n]
tableHeader = map (forceAttr (attrName "headerSet"))

drawMemory :: Address -> Machine -> Widget n
drawMemory addr state =
    renderTable
    . surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignRight
    . setDefaultRowAlignment AlignBottom
    . table
    $ header : rows
    where
        header = map (padBottom (Pad 0) . padLeft (Pad 2)) (emptyWidget : subAddresses 16)
        rows = take 30 (map (\a -> memoryRow 16 a state) [addr, addr+16 .. ])

subAddresses :: Int -> [Widget n]
subAddresses n =
    map (hex (attrName "header") (attrName "headerSet") 2 2) [0 .. n-1]

memoryRow :: Int -> Address -> Machine -> [Widget n]
memoryRow n addr state =
    hex (attrName "header") (attrName "headerSet") 
        6 6 
        (fromIntegral addr)
    : map 
        (\(row, byte) ->
            modifyDefAttr 
                (if pcLoc == row 
                    then (`withStyle` standout) 
                    else id) 
                (hex (attrName "register") (attrName "registerSet")
                    2 2 byte))
        bytes
    where
        pcLoc           = state^.pc
        rowAddresses    = [addr..addr+(fromIntegral n-1)]
        memorySect      = map (\a -> state^.byte a) rowAddresses
        bytes           = zip rowAddresses (map fromIntegral memorySect)

drawRegisters :: Machine -> Widget n
drawRegisters state = 
    renderTable
    . surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignLeft
    . setDefaultRowAlignment AlignBottom
    . table 
    . (map . map) (padLeft (Pad 2)) $ 

    [ tableHeader [str "", str "bin", str "hex", str "value"]
    , drawRegister24 "A" (state^.a)
    , drawRegister24 "X" (state^.x)
    , drawRegister24 "L" (state^.l)
    , drawRegister24 "S" (state^.s)
    , drawRegister24 "T" (state^.t)
    , drawRegister24 "B" (state^.b)
    , drawRegister24 "PC" (state^.pc)
    , drawSW (state^.sw)
    , drawRegisterFloat "F" (state^.f)
    ]

registerName :: String -> Widget n
registerName = withAttr (attrName "headerSet") . str

registerWrapSettings :: WrapSettings
registerWrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst
}

limitedString :: AttrName -> AttrName -> Char -> Int -> Int -> String -> Widget n
limitedString prefixAttr valueAttr prefix size limit s =
    hLimit limit
    $ 
    foldr (<=>) emptyWidget 
        (replicate 
            ((size - stringLength - 1) `div` limit)
            prefixWidget)
    <=>
        (prefixWidget 
        <+> 
        ( withAttr valueAttr
        . vLimit 2
        . hLimit stringLength
        . strWrapWith registerWrapSettings
        $ s))
    where
        stringLength = length s
        prefixWidget = vLimit 1
            . withAttr prefixAttr
            $ fill prefix

bin :: AttrName -> AttrName -> Int -> Int -> Int -> Widget n
bin prefixAttr valueAttr size limit n =
    limitedString
        prefixAttr
        valueAttr
        '0' size limit
        bin
    where
        bin = if n /= 0 
            then showBin n ""
            else ""

hex :: AttrName -> AttrName -> Int -> Int -> Int -> Widget n
hex prefixAttr valueAttr size limit n =
    limitedString
        prefixAttr
        valueAttr
        '0' size limit
        hex
    where
        hex = if n /= 0 
            then map toUpper $ showHex n ""
            else ""

drawRegisterFloat :: String -> SicFloat -> [Widget n]
drawRegisterFloat name value = 
    [ registerName name
    , bin (attrName "register") (attrName "registerSet") 48 24 intValue
    , hex (attrName "register") (attrName "registerSet") 12 6 intValue
    , withAttr (attrName "value") . str $ printf "%f" value]
    where
        intValue = fromIntegral $ convert @SicFloat @SicWord value

drawRegister :: String -> Int -> [Widget n]
drawRegister name value =
    [ registerName name
    , bin (attrName "register") (attrName "registerSet") 24 24 value
    , hex (attrName "register") (attrName "registerSet") 6 6 value
    ]

drawRegister24 :: String -> SicWord -> [Widget n]
drawRegister24 name value =
    drawRegister name (convert value) ++ [withAttr (attrName "value") . str $ printf "%d" value]

drawSW :: Ordering -> [Widget n]
drawSW sw =
    drawRegister "SW" (fromIntegral . fromEnum $ sw) ++ [withAttr (attrName "value") . str $ show sw]

chooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor s l = Nothing

startEvent :: EventM n SimState ()
startEvent = return ()

runStep :: EventM n SimState ()
runStep = do
    sim <- use simulator
    liftIO $ step sim
    history' <- liftIO $ peekQueue (history sim)
    newState history'
runUndo :: EventM n SimState ()
runUndo = do
    sim <- use simulator
    liftIO $ undo 1 sim
    history' <- liftIO $ peekQueue (history sim)
    newState history'

start :: EventM n SimState ()
start = do
    sim <- use simulator
    events <- use eventChan
    stdout <- use stdoutBuffer
    commandQueue <- liftIO $ runSimulator sim
    updaterId <- liftIO $ forkIO (uiUpdater sim stdout events)
    return ()
    
newState :: S.Seq Machine -> EventM n SimState ()
newState states' = do
    machineHistory .= states'

handleEvent :: BrickEvent Name SimEvent -> EventM Name SimState ()
handleEvent (AppEvent (New states out)) = newState states >> writeBytes out
handleEvent (VtyEvent (EvKey KEsc [])) = mode .= Normal
handleEvent e = do
    use mode >>= \case
        Normal -> handleCommand e
        Console -> zoom console $ handleEditorEvent e

handleCommand :: BrickEvent n e -> EventM n SimState ()
handleCommand (VtyEvent (EvKey (KChar 'i') [])) = mode .= Console
handleCommand (VtyEvent (EvKey (KChar 'q') [])) = halt
handleCommand (VtyEvent (EvKey (KChar 's') [])) = start
handleCommand (VtyEvent (EvKey (KChar 'n') [])) = runStep
handleCommand (VtyEvent (EvKey (KChar 'u') [])) = runUndo
handleCommand (VtyEvent (EvKey (KChar '=') [])) = memoryStart %= (+16)
handleCommand (VtyEvent (EvKey (KChar '-') [])) = memoryStart %= (\x -> if x > 0 then x - 16 else x)
handleCommand e = resizeOrQuit e

writeByte :: SicByte -> EventM n SimState ()
writeByte byte = console %= applyEdit (moveRight . insertChar (toChar byte))
    where 
        toChar = toEnum . fromIntegral

writeBytes :: S.Seq SicByte -> EventM n SimState ()
writeBytes bytes = if S.null bytes
    then return ()
    else console %= applyEdit edits
    where
        edits = foldr1 (.) (map (\b -> moveRight . insertChar (toChar b)) (toList bytes))
        toChar = toEnum . fromIntegral

uiUpdater :: Simulator -> Queue SicByte -> BChan SimEvent -> IO ()
uiUpdater simulator stdout eventChan = do
    threadDelay 30000
    states <- peekQueue queue
    out <- atomically $ flushQueue stdout
    notify states out

    isRunning <- readMVar (running simulator)
    when isRunning $ uiUpdater simulator stdout eventChan

    where 
        queue             = history simulator
        notify states out = unless (S.null states && S.null out) $ writeBChan eventChan (UI.New states out)