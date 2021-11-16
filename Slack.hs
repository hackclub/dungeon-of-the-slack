{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slack
  ( connect
  ) where

import           Relude                  hiding ( error
                                                , many
                                                )

import           Control.Lens
import           Data.Aeson
import           Network.Wreq

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Network.WebSockets
import           Wuss


data GetURLResponse = GetURLResponse
  { ok    :: Bool
  , error :: Maybe Text
  , url   :: Maybe Text
  }
  deriving Generic
instance FromJSON GetURLResponse


type Parser = Parsec Void Text

data URL = URL
  { host :: Text
  , path :: Text
  }

-- this is of course inflexible,
-- but it only has to handle what slack responds with
parseURL :: Parser URL
parseURL = do
  _     <- string "wss://"
  host' <- takeWhileP Nothing (/= '/')
  path' <- many anySingle

  return URL { host = host', path = fromString path' }


data SlackMessage = Hello | SlashCommand | Unknown deriving Show

instance FromJSON SlackMessage where
  parseJSON = withObject "SlackMessage" $ \v -> do
    typeName :: Text <- v .: "type"
    return $ case typeName of
      "hello"          -> Hello
      "slash_commands" -> SlashCommand
      _                -> Unknown

wsClient :: ClientApp ()
wsClient conn = do
  putStrLn "Connected!"
  forever $ receiveData conn >>= \msg -> do
    liftIO $ case (decode . encodeUtf8) (msg :: Text) of
      Nothing -> void . die $ "Failed to parse JSON"
      Just s  -> print (s :: SlackMessage)


connect :: Text -> Text -> IO ()
connect _ wsToken = do
  getURLRes <- asJSON =<< postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 wsToken])
    "https://slack.com/api/apps.connections.open"
    ([] :: [FormParam])

  case url (getURLRes ^. responseBody) of
    Nothing ->
      void
        . die
        . toString
        . ("Failed to get WebSocket URI with: " <>)
        . fromMaybe "(no error)"
        . error
        . (^. responseBody)
        $ getURLRes
    Just url' -> case runParser parseURL "" url' of
      Left e -> void . die . ("Failed to parse URI: " <>) . show $ e
      Right u ->
        runSecureClient (toString . host $ u) 443 (toString . path $ u) wsClient

  return ()
