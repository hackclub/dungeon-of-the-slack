{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( connect
  ) where

import           Relude                  hiding ( error
                                                , many
                                                )

import           Control.Lens
import           Data.Aeson                     ( FromJSON )
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

parseURL :: Parser URL
parseURL = do
  _     <- string "wss://"
  host' <- takeWhileP Nothing (/= '/')
  path' <- many anySingle

  return URL { host = host', path = fromString path' }


wsClient :: ClientApp ()
wsClient conn = do
  putStrLn "Connected!"
  forever $ receiveData conn >>= liftIO . putTextLn


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
