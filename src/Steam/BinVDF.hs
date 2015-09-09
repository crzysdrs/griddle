module Steam.BinVDF
    (
     vdfFindKey,
     readBinVDF,
     VDFNode(..)
    )
    where
import Control.Monad
import Data.Char
import Data.Maybe
import Debug.Trace
import Steam
import Text.ParserCombinators.Parsec as Parsec
data VDFNode = VDFNil | VDFKey VDFNode VDFNode | VDFArray [VDFNode] | VDFInt Int | VDFStr String deriving (Show)
data VDFType = VDFKeyT | VDFStrT | VDFIntT | VDFErrorT deriving (Enum)
             
steamVDF :: GenParser Char st VDFNode
steamVDF =
    do
      result <- vdfKeyValue
      Parsec.char '\0008' -- No idea why this extra backspace is needed.
      eof
      return result

vdfRValue :: Int -> GenParser Char st VDFNode
vdfRValue name =
    case toEnum name of
      VDFKeyT -> do
        r <- manyTill vdfKeyValue (lookAhead $ try (Parsec.char '\0008'))
        Parsec.char '\0008'
        return $ VDFArray r
      VDFStrT -> vdfString
      VDFIntT -> vdfInt        
      VDFErrorT -> fail $ "Invalid VDF Type Descriptor (Type " ++ show name  ++ ")"
        
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

readBinVDF :: String -> IO VDFNode
readBinVDF s = do
  input <- readFile s
  return $ case parse steamVDF s input of
    Left _ ->  VDFNil
    Right r -> r

vdfFindKey :: [String] -> VDFNode -> Maybe VDFNode
vdfFindKey [] v = Just v
vdfFindKey (x:xs) (VDFKey (VDFStr s) v)
           | map toLower x == map toLower s = vdfFindKey xs v
           | otherwise = Nothing
vdfFindKey _ (VDFArray []) = Nothing                      
vdfFindKey xs (VDFArray (e:es)) =
    case vdfFindKey xs e of
      Just x -> Just x
      Nothing -> vdfFindKey xs (VDFArray es)
vdfFindKey _ _ = Nothing

               
