{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( connect
  ) where

import           Relude                  hiding ( error )

import           Control.Concurrent             ( forkIO )
import           Control.Lens
import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import           Network.Wreq

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Network.WebSockets
import           Wuss


data GetConnResponse = GetConnResponse
  { ok    :: Bool
  , error :: Maybe Text
  , url   :: Maybe Text
  }
  deriving Generic
instance FromJSON GetConnResponse


type Parser = Parsec Void Text

data URL = URL
  { host :: Text
  , path :: Text
  }

parseURL :: Parser URL
parseURL = do
  _     <- string "wss://"
  host' <- takeWhileP Nothing (/= '/')
  path' <- takeWhileP Nothing (const True)

  return URL { host = host', path = path' }


wsClient :: ClientApp ()
wsClient conn = do
  putStrLn "Connected!"

  _ <- forkIO . forever $ receiveData conn >>= liftIO . putTextLn
  let loop = do
        loop
  void loop


connect :: Text -> Text -> IO ()
connect _ wsToken = do
  getConnRes <- asJSON =<< postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 wsToken])
    "https://slack.com/api/apps.connections.open"
    ([] :: [FormParam])

  case url (getConnRes ^. responseBody) of
    Nothing ->
      void
        . die
        . toString
        . ("Failed to get WebSocket URI with: " <>)
        . fromMaybe "(no error)"
        . error
        . (^. responseBody)
        $ getConnRes
    Just url' -> case runParser parseURL "" url' of
      Left e -> void . die . ("Failed to parse URI: " <>) . show $ e
      Right u ->
        runSecureClient (toString . host $ u) 443 (toString . path $ u) wsClient

  return ()
