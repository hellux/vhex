--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module VHex.Types
( Name(..)
, Layout
, CmdLine

, VHexConfig(..)
, cfgScrollOffL, cfgBytesPerRowMultipleL

, EditorState(..)
, esModeL, esWindowL, esFilePathL, esConfigL

, WindowState(..)
, wsBufferL, wsLayoutL, wsScrollPosL

, Mode(..)
, CmdLineMode(..)
, MsgType(..)
, MsgState(..)

, InsMode(..)
, InputState(..)
, isInputL, isNewByteL
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

import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

import VHex.Window.ByteView (ByteView)

data Name = EditorWindow
          | CmdCursor
          | InputCursor
          deriving (Eq, Ord)

type Layout = ListZipper ByteView
type CmdLine = ListZipper Char

data MsgType = InfoMsg | ErrorMsg deriving (Show)

data MsgState = MsgState
    { msgType :: MsgType 
    , msgContents :: String
    } deriving (Show)

data CmdLineMode = CmdNone (Maybe MsgState)
                 | CmdEx CmdLine

data InsMode = InsertMode | ReplaceMode deriving (Show)

data InputState = InputState
    { isInput :: ListZipper Char
    -- ^ Input contents and cursor
    , isNewByte :: Bool
    -- ^ Entered new byte.
    } deriving (Show)
suffixLenses ''InputState

data Mode = NormalMode CmdLineMode
          | InputMode InsMode InputState

data WindowState = WindowState
    { wsBuffer :: ByteZipper
    -- ^ Contents and cursor of (edited) opened file.
    , wsScrollPos :: Int
    -- ^ Offset to first visible byte.
    , wsLayout :: Layout
    -- ^ Specify layout of byte views
    }
suffixLenses ''WindowState

data VHexConfig = VHexConfig
    { cfgScrollOff :: Int
    , cfgBytesPerRowMultiple :: Int
    }
suffixLenses ''VHexConfig

data EditorState = EditorState
    { esMode :: Mode
    -- ^ Current editor mode.
    , esWindow :: WindowState
    -- ^ Editor window.
    , esFilePath :: Maybe FilePath
    -- ^ Path to opened file.
    , esConfig :: VHexConfig
    -- ^ User application options.
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
