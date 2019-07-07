module VHex.Editor (updateEditor) where

import Graphics.Vty.Input.Events (Event(..), Key(..))
import Brick.Types

import VHex.Types
import VHex.State.Buffer
    ( toBufferContext, fromBufferContext
    , curHori
    )

updateEditor :: Event -> WindowState -> EventM Name WindowState
updateEditor vtye ws = do
    bc <- toBufferContext ws
    let bc' = case vtye of
                EvKey KLeft [] -> curHori Up bc
    return $ fromBufferContext bc' ws
