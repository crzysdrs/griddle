module Steam.Shortcuts
    (
     SteamShortcut(..),
     readShortcuts,
     writeShortcuts
    )
    where
import Steam
import Steam.BinVDF
import Data.Maybe
    
shortcutKeyList :: [String]   
shortcutKeyList = ["appname", "Exe", "StartDir", "icon", "tags"]
shortcutStem :: [String]
shortcutStem = ["Shortcuts"]
               
data SteamShortcut = SteamShortcut {appName :: String, exe :: String, startDir :: String, icon :: String, tags :: [String]} deriving (Show)

bang :: [a] -> Int -> Maybe a
bang [] _ = Nothing
bang (x:_) 0 = Just x            
bang (_:xs) n = bang xs (n - 1)

vdfGetValue :: VDFNode -> Maybe VDFNode
vdfGetValue (VDFKey _ v) = Just v
vdfGetValue _ = Nothing

vdfChildren :: VDFNode -> [VDFNode]
vdfChildren (VDFArray cs) = cs
vdfChildren _ = []
                
vdf2shortcuts :: VDFNode -> Maybe [SteamShortcut]
vdf2shortcuts vdfnode = do
  stem <- vdfFindKey shortcutStem vdfnode
  let shortcuts =  map vdf2shortcut (vdfChildren stem)
  sequence $ filter isJust shortcuts

vdf2shortcut :: VDFNode -> Maybe SteamShortcut
vdf2shortcut v = do
  strippedIdx <- vdfGetValue v
  let nodes = map (\s -> vdfFindKey [s] strippedIdx) shortcutKeyList
  found <- sequence nodes
  let g i = do
        item <- found `bang` i
        getVDFString item
  (appText:exeText:startDirText:iconText:_) <- mapM g [0..3]
  tagNode <- found `bang` 4
  tagsNodes  <- mapM vdfGetValue $ vdfChildren tagNode
  tags <- mapM getVDFString tagsNodes
  Just SteamShortcut {appName = appText, exe = exeText, startDir = startDirText, icon = iconText, tags = tags}
    where getVDFString item =          
                case item of
                  (VDFStr s) -> Just s
                  _ -> Nothing

readShortcuts :: SteamID -> IO (Maybe [SteamShortcut])
readShortcuts s = do
  vdf <- readBinVDF (shortcutFileLoc s)
  return $ vdf2shortcuts vdf
                   
writeShortcuts :: [SteamShortcut] -> IO ()
writeShortcuts s = undefined
