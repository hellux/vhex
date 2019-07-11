{-# LANGUAGE TemplateHaskell #-}

module VHex.Input
( InputContext(..)
, Input(..), fromInput, toInput
) where

import Data.Word (Word8)

import Control.Monad.Reader

import Lens.Micro
import Lens.Micro.Mtl
import Brick.Types

import VHex.Buffer
import VHex.Types
import VHex.ByteView (ByteView)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

data InputContext = InputContext
    { icFromWord :: Word8 -> String
    , icToWord :: String -> Maybe Word8
    }
suffixLenses ''InputContext

type InputM = ReaderT InputContext BufferM

data Input = Input
    { iIs :: InputState 
    , iBuf :: Buffer
    }
suffixLenses ''Input

toInput :: InputState -> WindowState -> Input
toInput is ws = Input { iIs = is
                      , iBuf = toBuffer ws
                      }

fromInput :: Input -> EditorState -> EditorState
fromInput inp es = es & esModeL .~ InputMode (iIs inp)
                      & esWindowL %~ fromBuffer (iBuf inp)

inputLoad :: Input -> InputM Input
inputLoad i = do
    fw <- view icFromWordL
    let newInput = case bSelected (i^.iBufL) of
                    Nothing -> LZ.empty
                    Just w -> LZ.fromList (fw w)
    i & iIsL.isInputL .~ newInput & return
