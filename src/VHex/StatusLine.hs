module VHex.StatusLine (viewStatusLine) where

import VHex.Types
import VHex.Attributes (attrStatusLine)

import Brick.Types (Widget, Padding(Max))
import Brick.Widgets.Core (withAttr, str, padRight, hLimitPercent, hBox)

-- TODO more accurate percentage using dimensions of editor
scrollPercentage :: Int -> Int -> String
scrollPercentage 0 _ = "Top"
scrollPercentage scroll len = show p ++ "%" where
    p :: Int
    p = round $ (*100) $ (fromIntegral scroll :: Double) /
                         (fromIntegral len :: Double)

viewStatusLine :: Model -> Widget Name
viewStatusLine m = withAttr attrStatusLine $ hBox
    [ hLimitPercent 85 $ padRight Max $ str $ filePath m
    , padRight Max $ str $ show (cursorPos m)
    , str $ scrollPercentage (scrollPos m) (bufLen m)
    ]
