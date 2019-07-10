{-# LANGUAGE TemplateHaskell #-}

module VHex.Input
( InputContext
) where

import Data.Word (Word8)

import Control.Monad.Reader

import Lens.Micro
import Lens.Micro.Mtl
import Brick.Types (suffixLenses)

import VHex.Types
import VHex.ByteView (ByteView)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

import VHex.Buffer

data InputContext = InputContext
    { icFromWord :: Word8 -> String
    , icToWord :: String -> Maybe Word8
    , icBuffer :: BufferContext
    }
suffixLenses ''InputContext

type InputM = Reader InputContext

data Input = Input
    { is :: InputState 
    , buf :: Buffer
    }
suffixLenses ''Input

inputLoad :: Input -> InputM Input
inputLoad inp = do
    fw <- view icFromWordL
    let newInput = case bSelected (inp^.bufL) of
                    Nothing -> LZ.empty
                    Just w -> LZ.fromList (fw w)
    inp & isL.isInputL .~ newInput & return
