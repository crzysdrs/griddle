import Steam
import Steam.Shortcuts
import Config
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath.Posix
import qualified Text.Regex.Posix as RE
import Safe
import qualified Data.HashMap.Strict as HM
import           Data.String.Utils
import           Network.HTTP
import Data.Char
import InterpString
import System.Directory
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 as C

data InvokeProvider = InvokeProvider {invName :: String, invProvider :: Maybe GriddleProvider, invArgs :: HM.HashMap String String}

interpFuncs :: HM.HashMap String (String -> String)
interpFuncs = HM.fromList
              [
               ("upper", map toUpper),
               ("lower", map toLower),
               ("urlencode", urlEncode),
               ("reverse", reverse),
               ("lstrip", lstrip),
               ("rstrip", rstrip),
               ("strip", strip)
              ]

firstMatch :: (a -> Bool) -> [a] -> Maybe a
firstMatch f l = do
  let remains = dropWhile (not . f) l
  headMay remains

consoleGridProvider :: HM.HashMap String String -> IO Bool
consoleGridProvider vars = do
  let urlStr = "http://consolegrid.com/api/top_picture?console={urlencode{console}}&game={urlencode{game}}"
      urlEither = convertInterpString vars interpFuncs (parseInterpString urlStr)
      targetMaybe = HM.lookup "img" vars

  let url = case urlEither of
              Right r -> r
              Left l -> fail $ "consoleGridProvider: " ++ l
  let target = fromMaybe (fail "Console Grid Provider Requires img") targetMaybe
  putStrLn ("Request " ++ url)
  response <- simpleHTTP (getRequest url)
  code <- getResponseCode response
  when (code == (2,0,0)) $ do
    responseBody <- getResponseBody response
    putStrLn ("Request " ++ responseBody)
    imgResponse <- simpleHTTP (getRequest responseBody)
    imgCode <- getResponseCode imgResponse
    when (imgCode == (2,0,0)) $
         do
           imgBody <- getResponseBody imgResponse
           BS.writeFile target (C.pack imgBody)
           return ()
    return ()
  return True

localProvider :: HM.HashMap String String -> IO Bool
localProvider vars = do
  let lookups =
          do
            target <- HM.lookup "img" vars
            file <- HM.lookup "file" vars
            return (target, file)

  case lookups of
    Nothing -> fail "localProvider needs img and file"
    Just (target, file) ->
        do
          let exts = [".png", ".jpg", ".jpeg", ".gif"]
          let files = map (\e -> joinPath [takeDirectory file, takeBaseName file ++ e]) exts
          exists <- mapM doesFileExist files
          let found = zip files exists
          let match = firstMatch snd found
          case match of
            Just (f,_) -> copyFile f target
            Nothing -> return ()
          return $ or exists

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

applyAction :: SteamID -> Config -> GriddleRule -> FilePath -> Either String (SteamShortcut, InvokeProvider)
applyAction steamid c r p = do
  let action = HM.lookup (Config.action r) (actions c)
  action' <- case action of
               Nothing -> Left $ concat ["Can't find action ", maybeDefault (title r) "(Unnamed Rule)"]
               Just a -> Right a
  let args = case actionArgs r of
               Just args -> HM.toList args
               Nothing -> []
  let ruleVars = HM.fromList $
             [
              ("base", takeBaseName p),
              ("file", takeFileName p),
              ("ext", takeExtension p),
              ("exts", takeExtensions p),
              ("dir", takeDirectory p),
              ("parent", lastDef "NoParent" (splitPath $ takeDirectory p) ),
              ("match", p),
              ("action", Config.action r),
              ("rule", maybeDefault (title r) "(Unnamed Rule)"),
              ("steamid", show $ steamId64 steamid)
             ] ++ args
  let conv = convertInterpString ruleVars interpFuncs
  let interpHashEntry (k,v) = case val of
                                Left l -> Left l
                                Right r -> Right (k, r)
          where val = conv v
  let eitherHash xs = case xs of
                        Left l -> Left l
                        Right r -> Right $ HM.fromList r

  appName <- conv $ name action'
  exe <- conv $ actionCmd action'
  startDir <- conv $ maybeDefault (Config.startDir action') [InterpStr "dir"]
  icon <- conv $ maybeDefault (Config.icon action') [InterpStr ""]
  tags <- mapM conv (Config.tags action')

  let short = SteamShortcut {
    appName=appName,
    exe=exe,
    Steam.Shortcuts.startDir=startDir,
    Steam.Shortcuts.icon=icon,
    Steam.Shortcuts.tags="griddle":tags,
    Steam.Shortcuts.path=Nothing,
    hidden=Nothing
  }

  provider <- conv $ maybeDefault (provider action') [InterpStr "<undefined>"]
  let imgPath = joinPath [steamGridDir steamid, show (appid short) ++ ".jpg"]
  let hmList =  (++) [("file", [InterpStr p]), ("img", [InterpStr imgPath])] $ HM.toList $ maybeDefault (providerArgs action')  (HM.fromList [])
  providerArgs <- eitherHash $ mapM interpHashEntry hmList
  let foundProvider = HM.lookup provider (providers c)

  return (short, InvokeProvider {invName = provider, invProvider = foundProvider, invArgs = providerArgs})

runProvider :: InvokeProvider -> IO ()
runProvider p = do
  let target = HM.lookup "img" (invArgs p)
  let imgTarget = fromMaybe (fail "img must be provided to runProvider") target
  exists <- doesFileExist imgTarget
  unless exists $
       do
         success <- localProvider (invArgs p)
         if not success
           then do
             success2 <- consoleGridProvider (invArgs p)
             return ()
           else putStrLn "Local Provider Found for Some File"
  return ()

managedByGriddle :: SteamShortcut -> Bool
managedByGriddle s = "griddle" `elem` Steam.Shortcuts.tags s

manageShortcuts :: SteamID -> Config -> [(FilePath, GriddleRule)] -> IO ()
manageShortcuts steamID config matchedRules = do
  let matchedResults = mapM (\(f,r) -> applyAction steamID config r f) matchedRules
  let matchedResults' =
          case matchedResults of
            Left l -> error l
            Right r -> r
  let (newShorts', providers) = unzip matchedResults'
  mapM_ runProvider providers
  maybeOldShorts <- readShortcuts steamID
  let oldShorts = fromMaybe (fail "Unable to read shortcuts") maybeOldShorts
  let unmanaged = filter (not . managedByGriddle) oldShorts
  let allShorts = unmanaged ++ newShorts'
  --mapM_ (\short -> copyFile "/home/crzysdrs/downloads/mario64.jpg" (joinPath [steamGridDir steamID, show (appid short) ++ ".png"])) newShorts'
  putStrLn $ "Writing " ++ show (length unmanaged) ++ " unmanaged shortcuts too"
  writeShortcuts steamID allShorts

main :: IO ()
main = do
  ids <- getSteamIDs
  maybeConfig <- readConfig "config.json"
  config <- case maybeConfig of
              Nothing -> fail "Config file unable to be opened"
              Just c -> return c
  files <- mapM (\d -> listFiles d True) (dirs config)
  let files' = concat files
  putStrLn $ "Checking Files : " ++ show (length files')
  let (matched, unmatched) = partition (isJust . snd) $ zip files' $ map (compareRules config) files'
  mapM_ (\x -> putStrLn $ fst x ++ " did not match any rules (checked against " ++ show (length (rules config)) ++ " rule(s))") unmatched
  let justMatched = map (\(x,y) -> (x, fromJust y)) matched
  mapM_ (\x -> putStrLn $ fst x ++ " matched rule \"" ++ maybeDefault (title (snd x)) "Unnamed Rule" ++ "\"") justMatched
  mapM_ (\s -> manageShortcuts s config justMatched) ids
  return ()
