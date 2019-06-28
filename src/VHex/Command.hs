module VHex.Command ( openFile, saveFile
                    , infoMsg, errorMsg
                    , viewCmdLine
                    , updateCmd
                    ) where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)

import qualified Data.ByteString as B

import Lens.Micro ((&))

import Graphics.Vty.Input.Events (Event(..), Key(..))

import Brick.Main (continue, halt)
import Brick.Types
import Brick.Widgets.Core ((<+>), withAttr, str)
import Brick.Widgets.Edit (getEditContents
                          , Editor
                          , renderEditor
                          , handleEditorEvent
                          )

import qualified VHex.ByteZipper as BZ
import VHex.Types
import VHex.Attributes

openFile :: FilePath -> Model -> IO Model
openFile path m = do
    contents <- B.readFile path
    return m { filePath = path
             , fileContents = contents
             , buffer = BZ.byteZipper contents
             }

saveFile:: Model -> IO Model
saveFile m = do
    let contents = BZ.contents (buffer m)
        path = filePath m
    res <- try $ B.writeFile path contents :: IO (Either IOException ())
    case res of
        Left err -> m & errorMsg (show err) & return
        Right _ -> m { fileContents = contents }
                    & infoMsg
                        (show path ++ " " ++ show (bufLen m) ++ "B written")
                    & return

infoMsg :: String -> Model -> Model
infoMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Right msg }

errorMsg :: String -> Model -> Model
errorMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Left msg }

executeCmd :: String -> Model -> EventM Name (Next Model)
executeCmd "write" = saveFile >>> liftIO >=> continue
executeCmd "quit" = halt
executeCmd "w" = executeCmd "write"
executeCmd "q" = executeCmd "quit"
executeCmd cmd = errorMsg ("Invalid command: " ++ cmd) >>> continue

updateCmd :: Model -> Event -> Editor String Name -> EventM Name (Next Model)
updateCmd m vtye cmdLine =
    case vtye of
        EvKey KEsc   [] -> continue $ m { mode = NormalMode $ CmdNone Nothing }
        EvKey KEnter [] -> executeCmd (head $ getEditContents cmdLine) m
        _ -> do
            cmdLine' <- handleEditorEvent vtye cmdLine
            continue m { mode = NormalMode $ CmdEx cmdLine' }

viewCmdLine :: Model -> Widget Name
viewCmdLine m = case mode m of
    NormalMode cm -> case cm of
        CmdNone Nothing -> str " "
        CmdNone (Just (Left err)) ->
            withAttr attrError (str err) <+> withAttr attrDef (str " ")
        CmdNone (Just (Right info)) ->
            withAttr attrDef (str info)
        CmdEx cmdLine -> str ":" <+> renderEditor (str . head) True cmdLine
    InputMode im _ _ -> case im of
        ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
        InsertMode -> withAttr attrMode $ str "-- INSERT --"
