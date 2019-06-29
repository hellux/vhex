module VHex.Command ( openFile, saveFile
                    , infoMsg, errorMsg
                    , viewCmdLine
                    , updateCmd
                    ) where

import Control.Category ((>>>))
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

type Command = [String] -> Model -> EventM Name (Next Model)
type ArgCount = Int -> Bool

cmdQuit :: Command
cmdQuit _ = halt

cmdWrite :: Command
cmdWrite args m = m & saveFile path & liftIO >>= continue
    where path = case args of
                    [] -> filePath m
                    path' -> unwords path'

commands :: [(String, (Command, ArgCount))]
commands = [ ("write", (cmdWrite, (>=0)))
           , ("quit",  (cmdQuit,  (==0)))
           ]

aliases :: [(String, String)]
aliases = [ ("w", "write")
          , ("q", "quit")
          ]

openFile :: FilePath -> Model -> IO Model
openFile path m = do
    contents <- B.readFile path
    return m { filePath = path
             , fileContents = contents
             , buffer = BZ.byteZipper contents
             }

saveFile:: String -> Model -> IO Model
saveFile path m = do
    let contents = BZ.contents (buffer m)
    res <- try $ B.writeFile path contents :: IO (Either IOException ())
    case res of
        Left err -> m & errorMsg (show err) & return
        Right _ -> m { fileContents = contents }
                    & infoMsg
                        (show path ++ " " ++ show (bufLen m) ++ "B written")
                    & return

emptyMsg :: Model -> Model
emptyMsg m = m { mode = NormalMode $ CmdNone $ Nothing }

infoMsg :: String -> Model -> Model
infoMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Right msg }

errorMsg :: String -> Model -> Model
errorMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Left msg }

executeCmd :: [String] -> Model -> EventM Name (Next Model)
executeCmd [] = emptyMsg >>> continue
executeCmd (cmd:args) = case cmdValue of
    Nothing -> errorMsg ("Invalid command: " ++ cmd) >>> continue
    Just (func, argCount)
        | argCount (length args) -> func args
        | otherwise -> errorMsg ("Invalid number of arguments: "
                                ++ show (length args)) >>> continue
    where cmdFull = case lookup cmd aliases of
                        Nothing -> cmd
                        Just cmd' -> cmd'
          cmdValue = lookup cmdFull commands

updateCmd :: Model -> Event -> Editor String Name -> EventM Name (Next Model)
updateCmd m vtye cmdLine =
    case vtye of
        EvKey KEsc   [] -> m & emptyMsg & continue
        EvKey KEnter [] -> case getEditContents cmdLine of
                            (line:[]) -> m & executeCmd (words line)
                            _ -> m & continue
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
