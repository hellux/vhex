module VHex.StatusLine (viewStatusLine) where

import VHex.Types
import VHex.Attributes (attrStatusLine)

import Brick.Types (Widget)
import Brick.Widgets.Core (withAttr, str)

viewStatusLine :: Model -> Widget Name
viewStatusLine m = withAttr attrStatusLine $ str $
    filePath m ++ ": "
    ++ show (cursorPos m) ++ ", "
    ++ show (bufLen m) ++ "  "
    ++ case mode m of
        NormalMode _ -> ""
        InputMode im input nb ->
            "Input " ++ show im ++ " " ++ show input ++ " " ++ show nb
