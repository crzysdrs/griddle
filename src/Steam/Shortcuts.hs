module Steam.Shortcuts where
import Control.Monad
import Data.Char
import Data.Maybe
import Debug.Trace
import Steam
import Text.ParserCombinators.Parsec as Parsec
data VDFNode = VDFNil | VDFKey VDFNode VDFNode | VDFArray [VDFNode] | VDFInt Int | VDFStr String deriving (Show)
data VDFType = VDFStrT | VDFIntT | VDFArrT
             
data SteamShortcut = SteamShortcut {appName :: String, exe :: String, startDir :: String, icon :: String, tags :: [String]} deriving (Show)

steamVDF :: GenParser Char st VDFNode
steamVDF =
    do
      result <- vdfKeyValue
      Parsec.char '\0008' -- No idea why this extra backspace is needed.
      eof
      return result

vdfRValue :: Int -> GenParser Char st VDFNode
vdfRValue n =
    case n of
      0 -> do
        r <- manyTill vdfKeyValue (lookAhead $ try (Parsec.char '\0008'))
        Parsec.char '\0008'
        return $ VDFArray r
      1 -> vdfString
      2 -> vdfInt        
      n -> fail $ "Invalid VDF Type Descriptor (Type " ++ show n ++ ")"
        
vdfKeyValue :: GenParser Char st VDFNode
vdfKeyValue =
    do
      valtype <- anyToken
      n <- vdfString
      value <- vdfRValue (ord valtype)
      return $ VDFKey n value
 
vdfString :: GenParser Char st VDFNode
vdfString =
    do
      result <- many (noneOf "\0000")
      string "\0000" 
      return $ VDFStr result

vdfInt :: GenParser Char st VDFNode
vdfInt =
    do
      result <- count 4 anyToken
      return $ VDFInt 0

test :: IO VDFNode
test = do
  input <- readFile "amccallie-shortcuts.vdf"
  return $ case parse steamVDF "nothing" input of
    Left l ->  VDFNil
    Right r -> r

vdfFindKey :: [String] -> VDFNode -> Maybe VDFNode
vdfFindKey [] v = Just v
vdfFindKey (x:xs) (VDFKey (VDFStr s) v)
           | map toLower x == map toLower s = vdfFindKey xs v
           | otherwise = Nothing
vdfFindKey xs (VDFArray []) = Nothing                      
vdfFindKey xs (VDFArray (e:es)) =
    case vdfFindKey xs (VDFArray [e]) of
      Just x -> Just x
      Nothing -> vdfFindKey xs (VDFArray es)
vdfFindKey _ _ = Nothing

vdfChildren (VDFArray cs) = cs
vdfChildren _ = []
                
shortcutKeyList = ["appname", "Exe", "StartDir", "icon", "tags"]
shortcutStem = ["Shortcuts"]
               
vdf2shortcuts :: VDFNode -> Maybe [SteamShortcut]
vdf2shortcuts vdfnode = do
  stem <- vdfFindKey shortcutStem vdfnode
  let shortcuts =  map vdf2shortcut (vdfChildren stem)
  sequence $ filter isJust shortcuts

bang :: [a] -> Int -> Maybe a
bang [] n = Nothing
bang (x:xs) 0 = Just x            
bang (x:xs) n = bang xs (n - 1)
                
vdf2shortcut :: VDFNode -> Maybe SteamShortcut
vdf2shortcut v = do
  let nodes = map (\s -> vdfFindKey [s] v) shortcutKeyList
  found <- sequence nodes
  app <- found `bang` 0
  appText <- case app of
               (VDFStr s) -> Just s
               _ -> Nothing                       
  --Just SteamShortcut {appName = nodes !! 0, exe = nodes !! 1, startDir = nodes !! 2, icon = nodes !! 3, tags = nodes !! 4}
  Just SteamShortcut {appName = appText, exe = "test", startDir = "test", icon = "test", tags = ["test"]}
