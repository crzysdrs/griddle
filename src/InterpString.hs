module InterpString
    (
     parseInterpString,
     deparseInterpString,
     convertInterpString,
     InterpString(..),
     InterpNode(..),
     InterpTop(..)
    )
    where

import           Control.Monad
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.String.Utils
import           Debug.Trace

import           Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
data InterpNode = InterpCmd String InterpNode
                | InterpLookup String
                  deriving (Show, Eq)
data InterpTop = InterpStr String
               | InterpElem InterpNode
                 deriving (Show, Eq)

type InterpString = [InterpTop]

interpString :: Parser InterpString
interpString = many interpTop

interpTop :: Parser InterpTop
interpTop = try interpStr <|> interpElem

interpElem :: Parser InterpTop
interpElem = do
  node <- interpNode
  return $ InterpElem node

interpStr :: Parser InterpTop
interpStr = do
  str <- normalString
  return $ InterpStr str

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
      lookup <- interpNode
      string "}"
      return $ InterpCmd str lookup

interpLookup :: Parser InterpNode
interpLookup =
    do
      string "{"
      str <- normalString
      string "}"
      return $ InterpLookup str

escapedCharString :: Parser Char
escapedCharString = do
  string "\\"
  oneOf "{}"

normalString :: Parser String
normalString = do
  many1 (try escapedCharString <|> noneOf "{}")

parseInterpString :: String -> InterpString
parseInterpString str =
   case parse interpString "" str of
     Left e  -> error $ show e
     Right r -> r

deparseEscape :: String -> String
deparseEscape s = replace "}" "\\}" $ replace "{" "\\{" s

deparseInterpString :: InterpString -> String
deparseInterpString s = concatMap deparseInterpTop s

deparseInterpNode :: InterpNode -> String
deparseInterpNode (InterpCmd cmd i) = "{" ++ deparseEscape cmd ++ deparseInterpNode i ++ "}"
deparseInterpNode (InterpLookup l) = "{" ++ deparseEscape l++ "}"

deparseInterpTop :: InterpTop -> String
deparseInterpTop (InterpStr s) = deparseEscape s
deparseInterpTop (InterpElem n) = deparseInterpNode n

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

convertInterpString keys funcs s = do
  converted <- mapM (convertInterpTop keys funcs) s
  return $ concat converted
convertInterpTop  _ _ (InterpStr s) = return s
convertInterpTop keys funcs (InterpElem n) = convertInterpNode keys funcs n
