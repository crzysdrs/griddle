{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import           Data.HashMap.Lazy as HM

data GriddleProvider = GriddleProvider {
      provCmd :: String
    } deriving (Show)

instance FromJSON GriddleProvider where
    parseJSON = withObject "x" $ \o -> do
                                    provCmd <- o .: "cmd"
                                    return GriddleProvider {..}

data GriddleAction = GriddleAction {
      name :: String,
      actionCmd :: String,
      tags :: [String],
      startDir :: Maybe String,
      icon :: Maybe String,
      img :: Maybe String,
      provider :: Maybe String,
      providerArgs :: Maybe (HM.HashMap String String)
    } deriving (Show)

instance FromJSON GriddleAction where
    parseJSON = withObject "GriddleAction" $ \o ->
                do
                  name <- o .: "name"
                  actionCmd <- o .: "cmd"
                  tags <- o .: "tags"
                  startDir <- o .:? "startdir"
                  icon <- o .:? "icon"
                  img <- o .:? "img"
                  provider <- o .:? "provider"
                  providerArgs <- o .:? "provider_args"
                  return GriddleAction {..}

data GriddleRule = GriddleRule {
      title :: Maybe String,
      match :: String,
      action :: String
    } deriving (Show)

instance FromJSON GriddleRule where
    parseJSON = withObject "GriddleRule" $ \o ->
                do
                  title <- o .:? "title"
                  match <- o .: "match"
                  action <- o .: "action"
                  return GriddleRule {..}

data Config = Config {
      rules :: [GriddleRule],
      actions :: HM.HashMap String GriddleAction,
      providers :: Maybe (HM.HashMap String GriddleProvider),
      dirs :: [FilePath]
    } deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o ->
                do
                  rules <- o .: "rules"
                  actions <- o .: "actions"
                  providers <- o .:? "gridproviders"
                  dirs <- o .: "dirs"
                  return Config {..}

readConfig :: FilePath -> IO (Maybe Config)
readConfig s = do
  jsonStr <- BS.readFile s
  let decoded = eitherDecode jsonStr
  values <- case decoded of
                 Left l -> do
                   putStrLn l
                   return Nothing
                 Right r -> return $ Just r
  case values of
    Just v -> do
      let x = parseEither parseJSON v
      case x of
        Left l -> do
                 putStrLn l
                 return Nothing
        Right r -> return r
    Nothing -> return Nothing
