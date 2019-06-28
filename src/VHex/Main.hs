module VHex.Main (vhex) where

import System.Environment (getArgs)

import qualified Data.ByteString as B

import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import qualified VHex.ByteZipper as BZ
import qualified VHex.ByteView as BV
import VHex.Types (Model(..), Mode(..), CmdLineMode(..), Name)
import VHex.Editor (normalMode, inputMode, viewEditor)
import VHex.StatusLine (viewStatusLine)
import VHex.Command (updateCmd, viewCmdLine, openFile)
import VHex.Attributes (attributes)

initialModel :: Model
initialModel = Model
    { filePath = ""
    , fileContents = B.empty
    , buffer = BZ.empty
    , layout = [ BV.hex, BV.ascii1 ]
    , cursorFocus = 0
    , scrollPos = 0
    , mode = NormalMode (CmdNone Nothing)
    }

update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update m (VtyEvent vtye) = case mode m of
    NormalMode cm -> case cm of
        CmdEx cmdLine -> updateCmd m vtye cmdLine
        CmdNone _ -> normalMode vtye m >>= continue
    InputMode im _ _ -> inputMode im vtye m >>= continue
update m _ = continue m

view :: Model -> [Widget Name]
view m = [ viewEditor m <=> viewStatusLine m <=> viewCmdLine m ]

app :: App Model e Name
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const attributes
          }

vhex :: IO ()
vhex = do
    args <- getArgs
    model <- if not (null args)
                then openFile (head args) initialModel
                else return initialModel
    _ <- defaultMain app model
    return ()
