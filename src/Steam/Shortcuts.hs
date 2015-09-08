module Steam.Shortcuts where
import Text.ParserCombinators.Parsec
import Data.Char
    
data SteamShortcut = SteamShortcut {appName :: String, exe :: String, startDir :: String, icon :: String, tags :: [String]} deriving (Show)
                   
steamShortcutFile :: GenParser Char st [SteamShortcut]
steamShortcutFile =
    do
      result <- namedArray 0 "shortcuts" shortcut
      string "\0008"
      eof
      return result

namedArray :: Int -> String ->  GenParser Char st a -> GenParser Char st [a]
namedArray d name f =
    do
      string "\0000"
      string name
      string "\0000"
      result <- many (arrayElem d f)
      string "\0008"
      return result
             
arrayElem :: Int -> GenParser Char st a -> GenParser Char st a
arrayElem d f =
    do
      string [chr d]
      numericLit
      f
             
shortcut :: GenParser Char st SteamShortcut
shortcut =
    do appName <- namedString "appname"
       exeName <- namedString "exe"
       startDir <- namedString "StartDir"
       icon <- namedString "icon"
       icon <- namedString "ShortcutPath"
       hidden <- namedDataLen "Hidden" 4
       tags <- namedArray 1 "tags" stringLit
       let result = SteamShortcut {appName = appName, exe = exeName, startDir = startDir, icon = icon, tags = tags}
       string "\0008"
       return result
       
stringLit :: GenParser Char st String
stringLit = do
  result <- many (noneOf "\0000")
  string "\0000"
  return result

numericLit :: GenParser Char st String
numericLit =
    do
      result <- many digit
      string "\0000"
      return result
         
namedString :: String -> GenParser Char st String
namedString n =
    do
      string "\0001"
      string n
      string "\0000"
      stringLit
             
namedDataLen :: String -> Int -> GenParser Char st String
namedDataLen name n =
    do
      string "\0002"
      string name
      string "\0000"
      count n anyChar
