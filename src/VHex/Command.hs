module VHex.Command ( openFile, saveFile
                    , infoMsg, errorMsg
                    , viewCmdLine
                    , updateCmd
                    ) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)

import Data.List (isPrefixOf)
import qualified Data.ByteString as B

import Lens.Micro ((&))

import Graphics.Vty.Input.Events (Event(..), Key(..))

import Brick.Main (continue, halt)
import Brick.Types
import Brick.Widgets.Core (showCursor, (<+>), withAttr, str)

import qualified VHex.ByteZipper as BZ
import qualified VHex.ListZipper as LZ
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

matches :: String -> [String]
matches line = filter (isPrefixOf line) $ map fst commands

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

setLine :: String -> Model -> Model
setLine line m = case mode m of
    NormalMode (CmdEx _) ->
        m { mode = NormalMode (CmdEx (LZ.fromList line)) }
    _ -> m

tabComplete :: String -> Model -> Model
tabComplete line = case matches line of
    [match] -> setLine match
    _ -> id

updateCmdLine :: CmdLine -> Event -> CmdLine
updateCmdLine cl vtye = case vtye of
    EvKey KLeft     [] -> cl & LZ.left
    EvKey KRight    [] -> cl & LZ.right
    EvKey KBS       [] -> cl & LZ.pop
    EvKey (KChar c) [] -> cl & LZ.push c
    _ -> cl

updateCmd :: Model -> Event -> CmdLine -> EventM Name (Next Model)
updateCmd m vtye cmdLine = case vtye of
    EvKey KEsc         [] -> m & emptyMsg & continue
    EvKey KEnter       [] -> m & executeCmd (words line)
    EvKey (KChar '\t') [] -> m & tabComplete line & continue
    _ -> continue m { mode = NormalMode $ CmdEx (updateCmdLine cmdLine vtye) }
    where line = LZ.toList cmdLine


viewCmdLine :: Model -> Widget Name
viewCmdLine m = case mode m of
    NormalMode cm -> case cm of
        CmdNone Nothing -> str " "
        CmdNone (Just (Left err)) ->
            withAttr attrError (str err) <+> withAttr attrDef (str " ")
        CmdNone (Just (Right info)) ->
            withAttr attrDef (str info)
        CmdEx cmdLine ->
            let i = LZ.position cmdLine + 1
            in showCursor Cursor (Location (i,0))
                $ str (":" ++ LZ.toList cmdLine)
    InputMode im _ _ -> case im of
        ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
        InsertMode -> withAttr attrMode $ str "-- INSERT --"
