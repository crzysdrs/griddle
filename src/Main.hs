import Steam
import Steam.Shortcuts
import Config
import Data.List
import Data.Maybe
import System.Directory (getDirectoryContents,doesDirectoryExist)
import Control.Monad
import System.FilePath.Posix
import qualified Text.Regex.Posix as RE
import Safe
import qualified Data.HashMap.Strict as HM
import           Data.String.Utils
import           Network.HTTP.Base
import Data.Char
import InterpString
import Debug.Trace
import System.Directory

listFiles :: FilePath -> Bool -> IO [FilePath]
listFiles s r = do
  allfiles <- getDirectoryContents s
  let abspath = map (\f-> joinPath [s,f]) (filter (`notElem` [".", ".."]) allfiles)
  files <- filterM (fmap not . doesDirectoryExist) abspath
  dirs <- filterM doesDirectoryExist abspath
  subfiles <-  if r
               then mapM (\s -> listFiles s r) dirs
               else return []
  return $ reverse $ files ++ concat subfiles

compareRules :: Config -> FilePath -> Maybe GriddleRule
compareRules config p = do
  let cmp = zip (rules config) $ map ((RE.=~) p  . Config.match) (rules config)
  let match = dropWhile (not . snd) cmp
  first <- headMay match
  return $ fst first

maybeDefault :: Maybe a -> a -> a
maybeDefault (Just m) _ = m
maybeDefault Nothing d = d

applyAction :: Config -> GriddleRule -> FilePath -> Either String SteamShortcut
applyAction c r p = do
  let action = HM.lookup (Config.action r) (actions c)
  action' <- case action of
               Nothing -> Left $ concat ["Can't find action ", maybeDefault (title r) "(Unnamed Rule)"]
               Just a -> Right a
  let vars = HM.fromList
             [
              ("base", takeBaseName p),
              ("file", takeFileName p),
              ("ext", takeExtension p),
              ("exts", takeExtensions p),
              ("dir", takeDirectory p),
              ("parent", lastDef "NoParent" (splitPath $ takeDirectory p) ),
              ("match", p),
              ("action", Config.action r),
              ("rule", maybeDefault (title r) "(Unnamed Rule)")
             ]
  let funcs = HM.fromList
              [
               ("upper", map toUpper),
               ("lower", map toLower),
               ("urlencode", urlEncode),
               ("reverse", reverse),
               ("lstrip", lstrip),
               ("rstrip", rstrip),
               ("strip", strip)
              ]
  let conv = convertInterpString vars funcs

  appName <- conv $ name action'
  exe <- conv $ actionCmd action'
  startDir <- conv $ maybeDefault (Config.startDir action') (InterpLookup "dir")
  icon <- conv $ maybeDefault (Config.icon action') (InterpPlain "")
  tags <- mapM conv (Config.tags action')

  Right SteamShortcut {
    appName=appName,
    exe=exe,
    Steam.Shortcuts.startDir=startDir,
    Steam.Shortcuts.icon=icon,
    Steam.Shortcuts.tags="griddle":tags,
    Steam.Shortcuts.path=Nothing,
    hidden=Nothing
  }

managedByGriddle :: SteamShortcut -> Bool
managedByGriddle s = "griddle" `elem` Steam.Shortcuts.tags s

main :: IO ()
main = do
  ids <- getSteamIDs
  maybeConfig <- readConfig "config.json"
  config <- case maybeConfig of
              Nothing -> fail "Config file unable to be opened"
              Just c -> return c
  shortcuts <- mapM readShortcuts ids
  let p = zip ids shortcuts
  files <- mapM (\d -> listFiles d True) (dirs config)
  let files' = concat files
  putStrLn $ "Checking Files : " ++ show (length files')
  let (matched, unmatched) = partition (isJust . snd) $ zip files' $ map (compareRules config) files'
  mapM_ (\x -> putStrLn $ (fst x) ++ " did not match any rules (checked against " ++ show (length (rules config)) ++ " rule(s))") unmatched
  let justMatched = map (\(x,y) -> (x, fromJust y)) matched
  mapM_ (\x -> putStrLn $ (fst x) ++ " matched rule \"" ++ maybeDefault (title (snd x)) "Unnamed Rule" ++ "\"") justMatched
  let newshorts = mapM (\(f,r) -> applyAction config r f) justMatched
  let newshorts' = case newshorts of
                     Left l -> error l
                     Right r -> r
  putStrLn $ "Total Griddle Shortcuts: " ++ show (length newshorts')
  m <- mapM (\(x,y) -> case y of
    Just s -> do
               let oldshorts = filter (not . managedByGriddle) s
               let allshorts = oldshorts ++ newshorts'
               mapM_ (\short -> copyFile "/home/crzysdrs/downloads/mario64.jpg" (joinPath [steamGridDir x, show (appid short) ++ ".jpg"])) newshorts'
               putStrLn $ "Writing " ++ show (length oldshorts) ++ " unmanaged shortcuts too"
               writeShortcuts x allshorts
    Nothing -> putStrLn "Not Outputting" ) p
  return ()
