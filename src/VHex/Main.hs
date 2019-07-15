module VHex.Main (vhex) where

import System.Environment (getArgs)

import Graphics.Vty.Input.Events (Event(..))

import Brick.Main ( App(..)
                  , defaultMain
                  , continue
                  , invalidateCache
                  , showFirstCursor
                  )
import Brick.Types ( BrickEvent(..)
                   , EventM
                   , Next
                   , Widget
                   )
import Brick.Widgets.Core ((<=>))

import qualified VHex.ByteZipper as BZ
import qualified VHex.ListZipper as LZ
import VHex.Types ( EditorState(..)
                  , WindowState(..)
                  , VHexConfig(..)
                  , Mode(NormalMode)
                  , CmdLineMode(..)
                  , Name
                  )
import VHex.Command ( updateCmd
                    , viewStatusLine
                    , viewCmdLine
                    , openFile
                    )
import VHex.Attributes (attributes)
import VHex.Window (updateWindow, viewWindow)
import qualified VHex.Window.ByteView as BV

-- | Complete initial state of the application.
initialState :: EditorState
initialState = EditorState
    { esMode = NormalMode (CmdNone Nothing)
    , esWindow = WindowState
        { wsBuffer = BZ.empty
        , wsLayout = LZ.fromList [ BV.binary, BV.ascii1 ]
        , wsScrollPos = 0
        }
    , esFilePath = Nothing
    , esConfig = VHexConfig
        { cfgScrollOff = 5
        , cfgBytesPerRowMultiple = 4
        }
    }

-- | Update method a.k.a. event handler, receives the application state and an
-- event and produces the next application state.
update :: EditorState -> BrickEvent Name e -> EventM Name (Next EditorState)
update es (VtyEvent (EvResize _ _)) = invalidateCache >> continue es
update es (VtyEvent vtye) = case esMode es of
    NormalMode (CmdEx cmdState) -> updateCmd vtye cmdState es
    _                          -> updateWindow vtye es >>= continue
update es _ = continue es

-- | View or draw method, takes the entire state of the application and
-- produces an output screen.
view :: EditorState -> [Widget Name]
view es = [ viewWindow es <=> viewStatusLine es <=> viewCmdLine es ]

app :: App EditorState e Name
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const attributes
          }

-- | Main function, the entry point of the program. Loads a file if provided
-- and begins the update -> event -> view loop.
vhex :: IO ()
vhex = do
    args <- getArgs
    state <- if not (null args)
                then openFile (head args) initialState
                else return initialState
    _ <- defaultMain app state
    return ()
