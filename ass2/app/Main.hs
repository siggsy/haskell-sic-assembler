import Commands
import Machine hiding (get)
import Loader hiding (t)
import UI hiding (queue)
import System.Environment
import Brick
import Graphics.Vty
import Text.Printf
import Brick.BChan
import Control.Monad
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Pipes
import Pipes.Core
import Pipes.Lift
import qualified Pipes.Prelude as P
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Control.Concurrent as C
import qualified Data.Map as M
import Foreign
import System.IO
import Data.Maybe
import qualified GHC.Conc as Con
import qualified Data.Sequence as S

main :: IO ()
main = do
    (file : _) <- getArgs
    prog <- readFile file
    
    let sic = initialize
        app = App {
            appDraw = (:[]) <$> drawInterface,
            appHandleEvent = handleEvent,
            appChooseCursor = chooseCursor,
            appStartEvent = startEvent,
            appAttrMap = const attributeMap
        }

    eventChannel    <- newBChan 100
    sic'            <- S.execStateT (load prog) sic

    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    initialVty  <- buildVty
    initial     <- initialState sic' eventChannel
    finalState  <- customMain initialVty buildVty (Just eventChannel) app initial
    return ()
