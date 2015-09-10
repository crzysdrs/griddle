{-# LANGUAGE OverloadedStrings #-}
module Steam.BinVDF
    (
     readBinVDF,
     writeBinVDF
    )
    where
import           Control.Monad
import           Data.Aeson
import           Data.Binary
import qualified Data.ByteString as B
import           Data.Char
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Debug.Trace
import           Steam
import           Text.ParserCombinators.Parsec as Parsec
data VDFType = VDFKeyT | VDFStrT | VDFIntT | VDFErrorT deriving (Enum)

vdfObject :: [(T.Text, Value)] -> Value
vdfObject o = if fst (unzip o) == map (T.pack . show) [0..length o - 1]
              then Array $ V.fromList $ snd (unzip o)
              else object o

steamVDF :: GenParser Char st Value
steamVDF =
    do
      result <- vdfKeyValue
      Parsec.char '\0008' -- No idea why this extra backspace is needed.
      eof
      return $ vdfObject [result]

vdfRValue :: Int -> GenParser Char st Value
vdfRValue name =
    case toEnum name of
      VDFKeyT -> do
        r <- manyTill vdfKeyValue (lookAhead $ try (Parsec.char '\0008'))
        Parsec.char '\0008'
        return $ vdfObject r
      VDFStrT -> do
        s <- vdfString
        return $ String s
      VDFIntT -> vdfInt
      VDFErrorT -> fail $ "Invalid VDF Type Descriptor (Type " ++ show name  ++ ")"

vdfKeyValue :: GenParser Char st (T.Text, Value)
vdfKeyValue =
    do
      valtype <- anyToken
      n <- vdfString
      value <- vdfRValue (ord valtype)
      return (n, value)

vdfString :: GenParser Char st T.Text
vdfString =
    do
      result <- many (noneOf "\0000")
      string "\0000"
      return $ T.pack result

vdfInt :: GenParser Char st Value
vdfInt =
    do
      result <- count 4 anyToken
      return $ Number 0

readBinVDF :: String -> IO (Maybe Value)
readBinVDF s = do
  input <- readFile s
  return $ case parse steamVDF s input of
    Left _ ->  Nothing
    Right r -> Just r

writeBinVDF :: Value -> IO ()
writeBinVDF = undefined
