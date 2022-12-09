module Simulator 
    ( -- Queue
      Queue
    , writeQueue
    , flushQueue
    , peekQueue
    , newQueue
    , createQueue

    -- Simulator
    , Simulator
    , history
    , running

    , newSimulator
    , runSimulator
    
    , SimulatorHalt
    , step
    , undo

    -- Useful re-exports
    , atomically
    , threadDelay
    , readIORef
    )
where

import Machine
import Commands

import Control.Monad
import Data.IORef
import GHC.Conc
import Control.Concurrent

import Pipes
import Pipes.Core
import Pipes.Lift

import Data.Map as Map
import Data.Sequence as Seq


-- | Queue for simplex communication
data Queue a = SimQueue !(TVar (Seq.Seq a)) Int

newQueue :: Int -> IO (Queue a)
newQueue size = do
    write <- newTVarIO Seq.empty
    return (SimQueue write size)

createQueue :: Int -> a -> IO (Queue a)
createQueue size a = do
    write <- newTVarIO (a :<| Empty)
    return (SimQueue write size)

writeQueue :: Queue a -> a -> STM ()
writeQueue (SimQueue write size) q = do
    qs <- readTVar write

    writeTVar write $ 
        if Seq.length qs >= size
            then case viewr qs of
                EmptyR -> error "Impossible"
                qss :> _ -> q <| qss
            else q <| qs

fillQueue :: Queue a -> Seq a -> STM ()
fillQueue (SimQueue write size) qs = do
    writeTVar write qs

discardQueue :: Int -> Queue a -> STM (Seq.Seq a)
discardQueue n (SimQueue write size) = do
    qs <- readTVar write
    let (lq, rq) = Seq.splitAt n qs
    writeTVar write rq
    return lq

flushQueue :: Queue a -> STM (Seq.Seq a)
flushQueue (SimQueue write size) = do
    qs <- readTVar write
    unless (Seq.null qs) $ writeTVar write Seq.empty
    return qs

peekQueue :: Queue a -> IO (Seq.Seq a)
peekQueue (SimQueue write size) = do readTVarIO write

-- | Simulator commands
data SimulatorHalt = Stop
    deriving (Show, Eq)

-- | Data structure that defines "simulator"
data Simulator = Simulator 
    { machine :: IORef Machine
    , history :: Queue Machine
    , deviceMap :: IORef DeviceMap
    , running :: MVar Bool
    , frequency :: Int
    }

-- | Create new simulator
newSimulator :: Machine -> DeviceMap -> Int -> Int -> IO Simulator
newSimulator initialMachine deviceMap frequency size = do
    machine     <- newIORef initialMachine
    deviceMap   <- newIORef deviceMap
    running     <- newMVar False
    history     <- createQueue size initialMachine
    return $ Simulator {
        machine = machine,
        history = history,
        deviceMap = deviceMap,
        running = running,
        frequency = frequency
    }

runSimulator :: Simulator -> IO (Queue SimulatorHalt)
runSimulator simulator = do

    -- Thread communication
    commandQueue <- newQueue 1
    swapMVar (running simulator) True
    forkIO $ runner commandQueue
    return commandQueue

    where 
        runner commandQueue = do
            commands <- atomically $ flushQueue commandQueue
            case commands of
                Empty -> do
                    halt <- step simulator
                    if halt
                        then do
                            swapMVar (running simulator) False
                            return ()
                        else runner commandQueue

                (Stop :<| _) -> do
                    swapMVar (running simulator) False
                    return ()

step :: Simulator -> IO Bool
step simulator = do

    machine'    <- readIORef (machine simulator)
    let command     = parsedCommand . parseCommand $ machine'
        devices     = lift . deviceServer (deviceMap simulator)
        execution   = execStateP machine' command

    machine'' <- runEffect $ devices >\\ execution

    when (machine'' /= machine') $ do
        writeIORef (machine simulator) machine''
        atomically $ writeQueue (history simulator) machine''

    return $ machine'' == machine'


undo :: Int -> Simulator -> IO ()
undo n simulator = do
    let queue = history simulator

    currentHistory <- peekQueue queue
    let (state', history') = case Seq.drop (n-1) currentHistory of
            Empty               -> error "Invalid state"
            a :<| Empty         -> (a, Seq.fromList [a])
            a :<| b :<| Empty   -> (b, Seq.fromList [b])
            a :<| b :<| bs      -> (b, bs)

    writeIORef (machine simulator) state'
    atomically $ fillQueue queue history'
    return ()