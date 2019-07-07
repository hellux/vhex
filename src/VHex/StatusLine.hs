module VHex.StatusLine (viewStatusLine) where

import Data.Maybe (fromMaybe)

import Lens.Micro

import Brick.Types (Widget, Padding(Max))
import Brick.Widgets.Core (withAttr, str, padRight, hLimitPercent, hBox)

import VHex.Types
import VHex.Attributes (attrStatusLine)
import qualified VHex.ByteZipper as BZ

-- TODO more accurate percentage using dimensions of editor
scrollPercentage :: Int -> Int -> String
scrollPercentage 0 _ = "Top"
scrollPercentage scroll len = show p ++ "%" where
    p :: Int
    p = round $ (*100) $ (fromIntegral scroll :: Double) /
                         (fromIntegral len :: Double)

viewStatusLine :: EditorState -> Widget Name
viewStatusLine es = withAttr attrStatusLine $ hBox
    [ hLimitPercent 85 $ padRight Max $ str $ fromMaybe "" $ esFilePath es
    , padRight Max $ str $ show (BZ.location $ es^.esWindowL.wsBufferL)
    , str $ scrollPercentage (es^.esWindowL.wsScrollPosL) (BZ.length $ es^.esWindowL.wsBufferL)
    ]
