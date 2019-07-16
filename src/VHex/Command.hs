module VHex.Command ( openFile, saveFile
                    , commandMode
                    , updateCmd
                    , viewCmdLine
                    , viewStatusLine
                    ) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)

import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, intersperse)
import qualified Data.ByteString as B

import Lens.Micro

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))

import Brick.Main (continue, halt, invalidateCache)
import Brick.Types
import Brick.Widgets.Core ( showCursor
                          , (<+>)
                          , withAttr
                          , str
                          , padRight
                          , hLimitPercent
                          , hBox
                          )

import VHex.Window.ByteView (byteView)
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

cmdByteView :: Command
cmdByteView [bvStr] es = case lookup bvStr byteView of
    Nothing -> doneErr ("Byteview \"" ++ bvStr ++ "\" does not exist.") es
    Just bv -> invalidateCache >>
               (es & esWindowL.wsLayoutL %~ LZ.replace bv & done)
cmdByteView _ es = done es

_cmds :: [(String, String, Command, ArgCount)]
_cmds =
    [ ("w",     "write",    cmdWrite,       (>=0))
    , ("q",     "quit",     cmdQuit,        (==0))
    , ("bv",    "byteview", cmdByteView,    (==1))
    ]

-- | Association list for obtaining commands and argument count test from a
-- command name.
commands :: [(String, (Command, ArgCount))]
commands = map (\(_, name, cmd, argc) ->
                 (name, (cmd, argc)))
               _cmds

-- | Association list for obtaining command names from an alias.
aliases :: [(String, String)]
aliases = map (\(alias, name, _, _) ->
                (alias, name))
              _cmds

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
        Left err -> es & message ErrorMsg (show err) & return
        Right _ -> es & message InfoMsg
                        (show path ++ " " ++
                         show (BZ.length $ es^.esWindowL.wsBufferL) ++
                         "B written")
                      & return

commandMode :: Mode
commandMode = NormalMode $ CmdEx CmdState { csLine = LZ.empty
                                          , csSuggestions = Nothing
                                          }

message :: MsgType -> String -> EditorState -> EditorState
message t m = esModeL .~ (NormalMode $ CmdNone $ Just msgState) where
    msgState = MsgState { msgType = t, msgContents = m }

done :: EditorState -> EventM Name (Next EditorState)
done = esModeL .~ (NormalMode $ CmdNone Nothing) >>> continue

doneInfo :: String -> EditorState -> EventM Name (Next EditorState)
doneInfo msg = message InfoMsg msg >>> continue

doneErr :: String -> EditorState -> EventM Name (Next EditorState)
doneErr msg = message ErrorMsg msg >>> continue

executeCmd :: [String] -> EditorState -> EventM Name (Next EditorState)
executeCmd [] = done
executeCmd (cmd:args) = case cmdValue of
    Nothing -> doneErr ("Invalid command: " ++ cmd)
    Just (func, argCount)
        | argCount (length args) -> func args
        | otherwise -> doneErr ("Invalid number of arguments: "
                                ++ show (length args))
    where cmdFull = fromMaybe cmd $ lookup cmd aliases
          cmdValue = lookup cmdFull commands

complete :: CmdState -> CmdState
complete cs = case (matches . LZ.toList . csLine) cs of
    []  -> cs
    [m] -> cs & csLineL .~ (LZ.edge $ LZ.fromList m)
    ms  -> cs & csSuggestionsL ?~ LZ.fromList ms

editorOp :: Event -> CmdState -> EditorState -> EventM Name (Next EditorState)
editorOp vtye cs = case vtye of
    EvKey KEsc         []       -> done
    EvKey (KChar 'g')  [MCtrl]  -> done
    EvKey (KChar 'c')  [MCtrl]  -> done
    EvKey KEnter       []       -> executeCmd args
    EvKey (KChar 'j')  [MCtrl]  -> executeCmd args
    EvKey (KChar 'm')  [MCtrl]  -> executeCmd args
    _ -> esModeL .~ NormalMode (CmdEx (cmdOp vtye cs)) >>> continue
    where args = (words . LZ.toList . csLine) cs

cmdOp :: Event -> CmdState -> CmdState
cmdOp vtye = case vtye of
    EvKey (KChar '\t') [] -> complete
    _ -> csLineL %~ lineOp vtye

lineOp :: Event -> CmdLine -> CmdLine
lineOp vtye = case vtye of
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

updateCmd :: Event -> CmdState -> EditorState -> EventM Name (Next EditorState)
updateCmd = editorOp

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
        CmdEx cs ->
            let i = LZ.position (csLine cs) + 1
            in showCursor CmdCursor (Location (i, 0))
                $ str (":" ++ LZ.toList (csLine cs))
    InputMode im _ -> case im of
        ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
        InsertMode  -> withAttr attrMode $ str "-- INSERT --"

-- TODO more accurate percentage using dimensions of editor
scrollPercentage :: Int -> Int -> String
scrollPercentage 0 _ = "Top"
scrollPercentage scroll len = show p ++ "%" where
    p :: Int
    p = round $ (*100) $ (fromIntegral scroll :: Double) /
                         (fromIntegral len :: Double)

viewStatus :: EditorState -> Widget Name
viewStatus es = withAttr attrStatusLine $ hBox
    [ hLimitPercent 85 $ padRight Max
                       $ str
                       $ fromMaybe "[No Name]"
                       $ esFilePath es
    , padRight Max $ str
                   $ show (BZ.location
                   $ es^.esWindowL.wsBufferL)
    , str $ scrollPercentage (es^.esWindowL.wsScrollPosL)
                             (BZ.length $ es^.esWindowL.wsBufferL)
    ]

viewSuggestions :: CompleteSuggestions -> Widget Name
viewSuggestions sg = ( withAttr attrStatusLine
                     . hBox
                     . map str
                     . intersperse "  "
                     . LZ.toList
                     ) sg

viewStatusLine :: EditorState -> Widget Name
viewStatusLine es = case esMode es of
    NormalMode (CmdEx cs) -> case csSuggestions cs of
        Just sg -> viewSuggestions sg
        _ -> viewStatus es
    _ -> viewStatus es
