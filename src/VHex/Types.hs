--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module VHex.Types
( Name(..)
, Layout
, CmdLine

, EditorState(..)
, esModeL, esWindowL, esFilePathL

, WindowState(..)
, wsBufferL, wsLayoutL, wsScrollPosL

, Mode(..)
, CmdLineMode(..)
, MsgType(..)
, MsgState(..)

, InsMode(..)
, InputState(..)
, isModeL, isInputL, isNewByteL
) where

{-
import Control.Monad.State ( MonadState, StateT
                           , liftIO
                           , get, put
                           , runStateT
                           )
import Control.Monad.IO.Class (MonadIO)
-}

import Brick.Types (Next, EventM, suffixLenses)
import qualified Brick.Main as Brick

import VHex.ByteView (ByteView)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

data Name = EditorWindow
          | CmdCursor
          deriving (Eq, Ord)

type Layout = TreeZipper ByteView
type CmdLine = ListZipper Char

data MsgType = InfoMsg | ErrorMsg

data MsgState = MsgState
    { msgType :: MsgType 
    , msgContents :: String
    }

data CmdLineMode = CmdNone (Maybe MsgState)
                 | CmdEx CmdLine

data InsMode = InsertMode | ReplaceMode

data InputState = InputState
    { isMode :: InsMode
    -- ^ Input mode.
    , isInput :: ListZipper Char
    -- ^ Input contents and cursor
    , isNewByte :: Bool
    -- ^ Entered new byte.
    }
suffixLenses ''InputState

data Mode = NormalMode CmdLineMode
          | InputMode InputState

data WindowState = WindowState
    { wsByteView
    -- ^ Specify display and input of bytes.
    , wsScrollPos :: Int
    -- ^ Offset to first visible byte.
    , wsFilePath :: Maybe FilePath
    }
suffixLenses ''WindowState

data EditorState = EditorState
    { esMode :: Mode
    -- ^ Current editor mode.
    , esBuffers :: Map FilePath ByteZipper
    -- ^ Contents and cursors of opened files, potentially edited.
    , esWindows :: Layout
    -- ^ Windows, as well as their selection and ordering.
    }
suffixLenses ''EditorState

{-
newtype VHexM a =
    VHexM { fromVHexM :: (StateT VHState (EventM Name) a) }

instance Functor VHexM where
    fmap f (VHexM x) = VHexM (fmap f x)

instance Applicative VHexM where
    pure x = VHexM (pure x)
    VHexM f <*> VHexM x = VHexM (f <*> x)

instance Monad VHexM where
    return x = VHexM (return x)
    VHexM x >>= f = VHexM (x >>= \ x' -> fromVHexM (f x'))

instance MonadState EditorState VHexM where
    get = vhCurrentState `fmap` VHexM get
    put st = VHexM $ do
        s <- get
        put $ s { vhCurrentState = st }

instance MonadIO VHexM where
    liftIO = VHexM . liftIO

data VHState =
    VHState { vhCurrentState :: EditorState
            , vhNextAction :: EditorState -> EventM Name (Next EditorState)
            }

runVHEvent :: EditorState -> VHexM () -> EventM Name (Next EditorState)
runVHEvent esPrev (VHexM st) = do
    let vhSt = VHState { vhCurrentState = esPrev
                       , vhNextAction = Brick.continue
                       }
    ((), esNext) <- runStateT st vhSt
    (vhNextAction esNext) (vhCurrentState esNext)
-}
