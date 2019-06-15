{-# LANGUAGE TemplateHaskell #-}

import System.IO (FilePath)
import System.Environment (getArgs)

import Data.Text as T

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU

import Graphics.Vty.Input.Events

import Lens.Micro.TH
import Lens.Micro

import Brick.Main
import Brick.AttrMap
import Brick.Forms
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center (hCenter)

data ResourceName = EditorViewPortName
                  | CommandLineFormName
                  deriving (Show, Eq, Ord)

data CommandLineForm = CommandLineForm { _ex :: T.Text } deriving (Show)
data CommandLineMode = CommandNone
                     | CommandEx
                     | CommandSearch
                     deriving (Show)
data CommandState e = CommandState
    { _cmdMode :: CommandLineMode
    , _cmdForm :: Form CommandLineForm e ResourceName
    }
makeLenses ''CommandLineForm
makeLenses ''CommandLineMode
makeLenses ''CommandState

data FileState = FileState
    { _filePath :: FilePath
    , _fileContents :: ByteString
    }
    deriving (Show)
makeLenses ''FileState

data Model e = Model
    { _file :: FileState
    , _cmd :: (CommandState e)
    }
makeLenses ''Model

initialModel :: FilePath -> IO (Model e)
initialModel fname = do
    contents <- BL.readFile fname
    return $ Model
        { _file = FileState
                { _filePath = fname
                , _fileContents = contents
                }
        , _cmd = CommandState
               { _cmdMode = CommandNone 
               , _cmdForm = (mkForm $ CommandLineForm T.empty)
               }
        }

scroll :: Int -> Model e -> EventM ResourceName (Next (Model e))
scroll n m = vScrollBy (viewportScroll EditorViewPortName) n >> continue m

scrollTop m = vScrollToBeginning (viewportScroll EditorViewPortName) >> continue m
scrollBottom m = vScrollToEnd (viewportScroll EditorViewPortName) >> continue m

normalMode :: Model e -> Event -> EventM ResourceName (Next (Model e))
normalMode m vtye = 
    case vtye of
        EvKey (KChar 'q') [] -> halt m
        EvKey (KChar 'y') [MCtrl] -> scroll ( -1) m
        EvKey (KChar 'e') [MCtrl] -> scroll (  1) m
        EvKey (KChar 'u') [MCtrl] -> scroll (-15) m
        EvKey (KChar 'd') [MCtrl] -> scroll ( 15) m
        EvKey (KChar 'g') [] -> scrollTop m
        EvKey (KChar 'G') [] -> scrollBottom m
        _ -> continue m

exCommand :: Model e -> Event -> EventM ResourceName (Next (Model e))
exCommand m vtye =
    case vtye of
        EvKey KEsc [] -> continue $ m & cmd.cmdMode .~ CommandNone
        EvKey KEnter [] -> continue $ m & cmd.cmdMode .~ CommandNone
        _ -> do 
            cmdForm' <- handleFormEvent (VtyEvent vtye) (m ^. cmd.cmdForm)
            continue (m & cmd.cmdForm .~ cmdForm')

update :: Model e -> BrickEvent ResourceName e
       -> EventM ResourceName (Next (Model e))
update m e =
    case e of
        VtyEvent vtye -> 
            case m ^. cmd.cmdMode of
                CommandNone ->
                    case vtye of
                        EvKey (KChar ':') [] ->
                            continue (m & cmd.cmdMode .~ CommandEx)
                        _ ->
                            normalMode m vtye
                CommandEx -> exCommand m vtye
                CommandSearch -> continue m
        _ -> continue m

mkForm :: CommandLineForm -> Form CommandLineForm e ResourceName
mkForm = newForm [ 
    (str ":" <+>) @@= editTextField ex CommandLineFormName (Just 1)
    ]

viewEditor m = viewport EditorViewPortName Vertical $
    str (BLU.toString $ m ^. file.fileContents) 

viewCmdLine m =
    case m ^. cmd.cmdMode of
        CommandNone -> hCenter $ str " "
        CommandEx -> renderForm $ m ^. cmd.cmdForm
        CommandSearch -> hCenter $ str " "

view :: Model e -> [Widget ResourceName]
view m =
    [ viewEditor m <=> viewCmdLine m ]

app :: App (Model e) e ResourceName
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const $ attrMap mempty []
          }

main = do 
    args <- getArgs
    init <- initialModel (args !! 0)
    defaultMain app init
