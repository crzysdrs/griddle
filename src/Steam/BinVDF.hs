{-# LANGUAGE OverloadedStrings #-}
module Steam.BinVDF
    (
     readBinVDF,
     writeBinVDF
    )
    where
import           Control.Monad
import           Data.Aeson
import           Data.Scientific
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Debug.Trace
import           Steam
import           Text.ParserCombinators.Parsec as Parsec
data VDFType = VDFKeyT | VDFStrT | VDFIntT | VDFErrT deriving (Enum)

vdfObject :: [(T.Text, Value)] -> Value
vdfObject o = if fst (unzip o) == map (T.pack . show) [0..length o - 1]
              then Array $ V.fromList $ snd (unzip o)
              else object o

steamVDF :: GenParser Char st Value
steamVDF =
    do
      result <- vdfRValue (fromEnum VDFKeyT)
      eof
      return result

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
      _ -> fail $ "Invalid VDF Type Descriptor (Type " ++ show name  ++ ")"

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
      return $ Number $ scientific (toInteger $ bytes2Int result) 0

bytes2Int :: String -> Int
bytes2Int cs  = fromIntegral $ BinGet.runGet BinGet.getWord32le $ C.pack cs

int2Bytes :: Int -> String
int2Bytes i = C.unpack $ BinPut.runPut (BinPut.putWord32le (fromIntegral i))

readBinVDF :: String -> IO (Maybe Value)
readBinVDF s = do
  input <- readFile s
  return $ case parse steamVDF s input of
    Left _ ->  Nothing
    Right r -> Just r

writeBinVDF :: FilePath -> Value -> IO ()
writeBinVDF p v = writeFile p (jsonVDF v)


writeVDFStr :: T.Text -> String
writeVDFStr s = T.unpack s ++ "\0000"

jsonVDFEnum :: Value -> VDFType
jsonVDFEnum (Object _) = VDFKeyT
jsonVDFEnum (Array _) = VDFKeyT
jsonVDFEnum (String _) = VDFStrT
jsonVDFEnum (Number _) = VDFIntT
jsonVDFEnum _ = error "JSONVDF Type Enum Should Not be Used"

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

jsonFixedOrder :: [(T.Text, Value)] -> String
jsonFixedOrder ts = concatMap pair (filter removeNull ts) ++ "\0008"
    where pair (t,v) = [chr (fromEnum $ jsonVDFEnum v)] ++ writeVDFStr t ++ jsonVDF v
          removeNull (t,v) = not $ isNull v
jsonVDF :: Value -> String
jsonVDF (Object h) = jsonFixedOrder (HM.toList h)
jsonVDF (Array vec) = jsonFixedOrder $ zip (map (T.pack . show) [0..]) (V.toList vec)
jsonVDF (String s) = writeVDFStr s
jsonVDF (Number i) = int2Bytes (case floatingOrInteger i of
                                  Left l -> error "Can't write Floating Point"
                                  Right r -> r)
jsonVDF _ = error "JSONVDF Type Should Not Be Emitted"
