{-# LANGUAGE TemplateHaskell #-}

module VHex.State.Input
( InputContext
) where

import Data.Word (Word8)

import Lens.Micro
import Brick.Types (suffixLenses)

import VHex.Types
import VHex.ByteView (ByteView)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

import VHex.State.Buffer (BufferContext, bcSelected)

data InputContext = InputContext
    { icIs :: InputState 
    , icBc :: BufferContext
    , icFromWord :: Word8 -> String
    , icToWord :: String -> Maybe Word8
    }

suffixLenses ''InputContext

inputLoad :: InputContext -> InputContext
inputLoad ic = ic & icIsL.isInputL .~ newInput where
    newInput = case bcSelected (ic^.icBcL) of
            Nothing -> LZ.empty
            Just w -> LZ.fromList $ (ic^.icFromWordL) w
