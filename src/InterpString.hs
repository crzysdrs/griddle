module InterpString
    (
     parseInterpString,
     deparseInterpString,
     convertInterpString,
     InterpString(..),
     InterpNode(..)
    )
    where

import           Control.Monad
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.String.Utils
import           Debug.Trace
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
data InterpNode = InterpCmd String InterpNode
                | InterpLookup String
                  deriving (Show, Eq)
data InterpString = InterpJoin InterpNode InterpString
                  | InterpPlain String
                    deriving (Show, Eq)



interpString :: Parser InterpString
interpString = try interpJoin <|> identString

interpJoin :: Parser InterpString
interpJoin = do
  is <-interpNode
  str <- interpString
  return $ InterpJoin is str

interpNode :: Parser InterpNode
interpNode = do
  node <- try cmdString
          <|> interpLookup
  return $ node

cmdString :: Parser InterpNode
cmdString =
    do
      string "{"
      str <- normalString
      lookup <- try interpNode
      string "}"
      return $ InterpCmd str lookup

interpLookup :: Parser InterpNode
interpLookup =
    do
      string "{"
      str <- normalString
      string "}"
      return $ InterpLookup str

identString :: Parser InterpString
identString = do
  str <- normalString
  return $ InterpPlain str

escapedCharString :: Parser Char
escapedCharString = do
  string "\\"
  oneOf "{}"

normalString :: Parser String
normalString = many1 (try escapedCharString <|> noneOf "{}")

parseInterpString :: String -> InterpString
parseInterpString str =
   case parse interpString "" str of
     Left e  -> error $ show e
     Right r -> r

deparseEscape :: String -> String
deparseEscape s = replace "}" "\\}" $ replace "{" "\\{" s

deparseInterpString :: InterpString -> String
deparseInterpString (InterpPlain s) = deparseEscape s
deparseInterpString (InterpJoin n s) = deparseInterpNode n ++ deparseInterpString s

deparseInterpNode :: InterpNode -> String
deparseInterpNode (InterpCmd cmd i) = "{" ++ deparseEscape cmd ++ deparseInterpNode i ++ "}"
deparseInterpNode (InterpLookup l) = "{" ++ deparseEscape l++ "}"

convertInterpString :: HM.HashMap String String -> HM.HashMap String (String -> String) -> InterpString -> Either String String
convertInterpNode keys funcs (InterpCmd cmd i) = do
  let maybeF = HM.lookup cmd funcs
  f <- case maybeF of
    Nothing -> Left $ concat ["Lookup Function \"", cmd, "\" not found"]
    Just f -> return f
  val <- convertInterpNode keys funcs i
  return $ f val
convertInterpNode  keys _ (InterpLookup l) =
  case HM.lookup l keys of
    Nothing -> Left $ concat ["Specified variable \"", l, "\" not found."]
    Just v -> return v
convertInterpString  _ _ (InterpPlain s) = return s
convertInterpString keys vals (InterpJoin n s) = do
  n' <- convertInterpNode keys vals n
  s' <- convertInterpString keys vals s
  return $ n' ++ s'
