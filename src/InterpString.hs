module InterpString
    (
     parseInterpString,
     convertInterpString,
     interpFuncs,
     InterpString(..)
    )
    where

import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.String.Utils
import           Debug.Trace
import           Network.HTTP.Base
import           Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data InterpString = InterpPlain String
                  | InterpCmd String InterpString
                  | InterpLookup String
                  | InterpArr [InterpString]
                    deriving (Show, Eq)

interpString :: Parser InterpString
interpString = do
  all <- many $ try identString
               <|> try cmdString
               <|> interpLookup
  return $ InterpArr all

cmdString :: Parser InterpString
cmdString =
    do
      string "{"
      str <- normalString
      lookup <- try cmdString <|> interpLookup
      string "}"
      return $ InterpCmd str lookup

interpLookup :: Parser InterpString
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
  c <- oneOf "{}"
  return $ c

normalString :: Parser String
normalString = do
  str <- many1 (try escapedCharString <|> noneOf "{}")
  return $ str

parseInterpString :: String -> InterpString
parseInterpString str =
   case parse interpString "" str of
     Left e  -> error $ show e
     Right r -> r

interpFuncs :: [(String, (String -> String))]
interpFuncs = [("upper", map toUpper),
               ("lower", map toLower),
               ("urlencode", urlEncode),
               ("reverse", reverse),
               ("lstrip", lstrip),
               ("rstrip", rstrip),
               ("strip", strip)
              ]

convertInterpString :: HM.HashMap String String -> HM.HashMap String (String -> String) -> InterpString -> Either String String
convertInterpString keys funcs (InterpArr arr) = do
  arrs <- mapM (convertInterpString keys funcs) arr
  return $ concat arrs
convertInterpString keys funcs (InterpCmd cmd i) = do
  let maybeF = HM.lookup cmd funcs
  f <- case maybeF of
    Nothing -> Left $ concat ["Lookup Function \"", cmd, "\" not found"]
    Just f -> return f
  val <- convertInterpString keys funcs i
  return $ f val
convertInterpString keys _ (InterpLookup l) = do
  case HM.lookup l keys of
    Nothing -> Left $ concat ["Specified variable \"", l, "\" not found."]
    Just v -> return v
convertInterpString  _ _ (InterpPlain s) = return s
