{-|
Module      : VHex.Main
Description : Global types and complete application state.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

{-# LANGUAGE TemplateHaskell #-}

module VHex.Types
( Name(..)
, EditorState(..)
-- ** Lenses
, esModeL, esWindowL, esFilePathL, esConfigL

-- * Configuration state
, VHexConfig(..)
, cfgScrollOffL, cfgBytesPerRowMultipleL

-- * Window state
, WindowState(..)
, Layout
-- ** Lenses
, wsBufferL, wsLayoutL, wsScrollPosL

-- * Mode state
, Mode(..)
, CmdLine, CompleteSuggestions
, CmdState(..)
, csLineL, csSuggestionsL
, CmdLineMode(..)
, MsgType(..)
, MsgState(..)
, InsMode(..)
, InputState(..)
-- ** Lenses
, isInputL, isNewByteL
) where

import Brick.Types (suffixLenses)

import VHex.ByteZipper (ByteZipper)
import VHex.ListZipper (ListZipper)

import VHex.Window.ByteView (ByteView)

data Name = EditorWindow
          | CmdCursor
          | InputCursor
          | CachedRow ByteView Int
          deriving (Eq, Ord)

type Layout = ListZipper ByteView

data MsgType = InfoMsg | ErrorMsg deriving (Show)

data MsgState = MsgState
    { msgType :: MsgType
    , msgContents :: String
    } deriving (Show)

type CmdLine = ListZipper Char
type CompleteSuggestions = ListZipper String

data CmdState = CmdState
    { csLine :: CmdLine
    , csSuggestions :: Maybe CompleteSuggestions
    } deriving (Show)
suffixLenses ''CmdState

data CmdLineMode = CmdNone (Maybe MsgState)
                 | CmdEx CmdState
                 deriving (Show)

data InsMode = InsertMode | ReplaceMode deriving (Eq, Show)

data InputState = InputState
    { isInput :: ListZipper Char
    -- ^ Input contents and cursor
    , isNewByte :: Bool
    -- ^ Entered new byte.
    } deriving (Show)
suffixLenses ''InputState

data Mode = NormalMode CmdLineMode
          | InputMode InsMode InputState
          deriving (Show)

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
    -- ^ Minimal number of lines to keep between cursor and edge of screen.
    , cfgBytesPerRowMultiple :: Int
    -- ^ Keep number of bytes per line a multiple of this value. E.g if there
    -- is space for 9 bytes but the multiple is set to 4, then there will be
    -- only 8 lines per row.
    }
suffixLenses ''VHexConfig

-- | Entire state of the application.
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
