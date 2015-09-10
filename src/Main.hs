import Steam
import Steam.Shortcuts
import Data.List
import Data.Maybe

main :: IO ()
main = do
  ids <- getSteamIDs
  shortcuts <- mapM readShortcuts ids
  let p = zip ids shortcuts
  m <- mapM (\(x,y) -> case y of
    Just s -> writeShortcuts x s
    Nothing -> putStrLn "Not Outputting" ) p
  return ()
