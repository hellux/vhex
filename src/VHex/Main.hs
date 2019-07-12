module VHex.Main (vhex) where

import System.Environment (getArgs)

import Lens.Micro

import Graphics.Vty.Input.Events (Event(..), Key(..))

import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import qualified VHex.ByteZipper as BZ
import qualified VHex.ListZipper as LZ
import qualified VHex.ByteView as BV
import VHex.Types
import VHex.Window
import VHex.StatusLine (viewStatusLine)
import VHex.Command (updateCmd, viewCmdLine, openFile)
import VHex.Attributes (attributes)

initialState :: EditorState
initialState = EditorState
    { esMode = NormalMode (CmdNone Nothing)
    , esWindow = WindowState
        { wsBuffer = BZ.empty
        , wsLayout = LZ.fromList [ BV.hex, BV.ascii1 ]
        , wsScrollPos = 0
        }
    , esFilePath = Nothing
    , esConfig = VHexConfig
        { cfgScrollOff = 5
        , cfgBytesPerRowMultiple = 4
        }
    }

commandMode :: Mode
commandMode = NormalMode $ CmdEx $ LZ.fromList ""

update :: EditorState -> BrickEvent Name e -> EventM Name (Next EditorState)
update es (VtyEvent vtye) = case esMode es of
    NormalMode (CmdEx cmdLine) -> updateCmd es vtye cmdLine
    _ -> case vtye of
        EvKey (KChar ':') [] -> es & esModeL .~ commandMode & continue
        _ -> updateWindow vtye es >>= continue
update es _ = continue es

view :: EditorState -> [Widget Name]
view es = [ viewWindow es <=> viewStatusLine es <=> viewCmdLine es ]

app :: App EditorState e Name
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const attributes
          }

vhex :: IO ()
vhex = do
    args <- getArgs
    state <- if not (null args)
                then openFile (head args) initialState
                else return initialState
    _ <- defaultMain app state
    return ()
