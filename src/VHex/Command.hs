module VHex.Command ( openFile, saveFile
                    , infoMsg, errorMsg
                    , viewCmdLine
                    , updateCmd
                    ) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)

import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import qualified Data.ByteString as B

import Lens.Micro

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))

import Brick.Main (continue, halt)
import Brick.Types
import Brick.Widgets.Core (showCursor, (<+>), withAttr, str)

import qualified VHex.ByteZipper as BZ
import qualified VHex.ListZipper as LZ
import VHex.Types
import VHex.Attributes

type Command = [String] -> EditorState -> EventM Name (Next EditorState)
type ArgCount = Int -> Bool

cmdQuit :: Command
cmdQuit _ = halt

cmdWrite :: Command
cmdWrite args es = es & saveFile path & liftIO >>= continue
    where path = case args of
                    [] -> fromMaybe "" (esFilePath es)
                    path' -> unwords path'

commands :: [(String, (Command, ArgCount))]
commands = [ ("write",      (cmdWrite,      (>=0)))
           , ("quit",       (cmdQuit,       (==0)))
           ]

aliases :: [(String, String)]
aliases = [ ("w", "write")
          , ("q", "quit")
          ]

matches :: String -> [String]
matches line = filter (isPrefixOf line) $ map fst commands

openFile :: FilePath -> EditorState -> IO EditorState
openFile path es = do
    contents <- B.readFile path
    return $ es & esFilePathL ?~ path
                & esWindowL.wsBufferL .~ BZ.byteZipper contents

saveFile:: String -> EditorState -> IO EditorState
saveFile path es = do
    let contents = BZ.contents (es^.esWindowL.wsBufferL)
    res <- try $ B.writeFile path contents :: IO (Either IOException ())
    case res of
        Left err -> es & errorMsg (show err) & return
        Right _ -> es & infoMsg
                        (show path ++ " " ++
                         show (BZ.length $ es^.esWindowL.wsBufferL) ++
                         "B written")
                      & return

emptyMsg :: EditorState -> EditorState
emptyMsg = esModeL .~ (NormalMode $ CmdNone Nothing)

message :: MsgType -> String -> EditorState -> EditorState
message t m = esModeL .~ (NormalMode $ CmdNone $ Just msgState) where
    msgState = MsgState { msgType = t, msgContents = m }

infoMsg :: String -> EditorState -> EditorState
infoMsg = message InfoMsg

errorMsg :: String -> EditorState -> EditorState
errorMsg = message ErrorMsg

executeCmd :: [String] -> EditorState -> EventM Name (Next EditorState)
executeCmd [] = emptyMsg >>> continue
executeCmd (cmd:args) = case cmdValue of
    Nothing -> errorMsg ("Invalid command: " ++ cmd) >>> continue
    Just (func, argCount)
        | argCount (length args) -> func args
        | otherwise -> errorMsg ("Invalid number of arguments: "
                                ++ show (length args)) >>> continue
    where cmdFull = fromMaybe cmd $ lookup cmd aliases
          cmdValue = lookup cmdFull commands

setLine :: String -> EditorState -> EditorState
setLine line es = case esMode es of
    NormalMode (CmdEx _) ->
        es { esMode = NormalMode (CmdEx (LZ.edge $ LZ.fromList line)) }
    _ -> es

tabComplete :: String -> EditorState -> EditorState
tabComplete line = case matches line of
    [match] -> setLine match
    _ -> id

updateCmdLine :: Event -> CmdLine -> CmdLine
updateCmdLine vtye = case vtye of
    EvKey KLeft       []        -> LZ.left
    EvKey KRight      []        -> LZ.right
    EvKey KBS         []        -> LZ.pop
    EvKey (KChar 'h') [MCtrl]   -> LZ.pop
    EvKey (KChar 'a') [MCtrl]   -> LZ.beginning
    EvKey (KChar 'b') [MCtrl]   -> LZ.beginning
    EvKey (KChar 'e') [MCtrl]   -> LZ.edge
    EvKey (KChar 'd') [MCtrl]   -> LZ.remove
    EvKey (KChar c)   []        -> LZ.push c
    _                           -> id

updateCmd :: Event -> CmdLine -> EditorState -> EventM Name (Next EditorState)
updateCmd vtye cmdLine = case vtye of
    EvKey KEsc         []       -> emptyMsg >>> continue
    EvKey (KChar 'g')  [MCtrl]  -> emptyMsg >>> continue
    EvKey (KChar 'c')  [MCtrl]  -> emptyMsg >>> continue
    EvKey KEnter       []       -> executeCmd (words line)
    EvKey (KChar 'j')  []       -> executeCmd (words line)
    EvKey (KChar 'm')  []       -> executeCmd (words line)
    EvKey (KChar '\t') []       -> tabComplete line >>> continue
    _ -> esModeL .~ NormalMode (CmdEx (updateCmdLine vtye cmdLine))
     >>> continue
    where line = LZ.toList cmdLine

viewCmdLine :: EditorState -> Widget Name
viewCmdLine es = case esMode es of
    NormalMode cm -> case cm of
        CmdNone Nothing -> str " "
        CmdNone (Just msg) ->
            let attr = case msgType msg of
                        InfoMsg -> attrDef
                        ErrorMsg -> attrError
            in withAttr attr (str $ msgContents msg) <+>
               withAttr attrDef (str " ")
        CmdEx cmdLine ->
            let i = LZ.position cmdLine + 1
            in showCursor CmdCursor (Location (i,0))
                $ str (":" ++ LZ.toList cmdLine)
    InputMode im _ -> case im of
        ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
        InsertMode -> withAttr attrMode $ str "-- INSERT --"
