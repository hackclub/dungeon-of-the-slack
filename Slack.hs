{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slack
  ( wsConnect
  , EventHandler
  , SocketEventContent(..)
  , SocketEventResContent(..)
  ) where

import           Relude                  hiding ( error
                                                , many
                                                )

import           Control.Lens            hiding ( (.=) )
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


data SocketEvent = SocketEvent
  { inEnvId :: Maybe Text
  , content :: SocketEventContent
  }
  deriving Show
data SocketEventContent = Hello | SlashCommand {scText :: Text} | Unknown deriving Show

instance FromJSON SocketEvent where
  parseJSON = withObject "SlackMessage" $ \v -> do
    typeName :: Text <- v .: "type"
    smContent        <-
      (case typeName of
        "hello"          -> return Hello
        "slash_commands" -> do
          text <- (v .: "payload") >>= (.: "text")
          return $ SlashCommand { scText = text }
        _ -> return Unknown
      )

    envId' :: Maybe Text <- v .:? "envelope_id"
    return $ SocketEvent envId' smContent


data SocketEventRes = SocketEventRes
  { outEnvId   :: Text
  , resContent :: SocketEventResContent
  }
data SocketEventResContent = SlashCommandRes
  { scrText      :: Text
  , scrInChannel :: Bool
  }

instance ToJSON SocketEventRes where
  toJSON om = object
    [ "envelope_id" .= outEnvId om
    , "payload" .= object
      [ "text" .= (scrText . resContent) om
      , "response_type"
        .= (if (scrInChannel . resContent) om
             then "in_channel"
             else "ephemeral" :: Text
           )
      ]
    ]


type EventHandler = SocketEventContent -> IO SocketEventResContent

wsClient :: EventHandler -> ClientApp ()
wsClient handleMsg conn = do
  putStrLn "Connected!"
  forever $ receiveData conn >>= \msg -> do
    liftIO $ case (decode . encodeUtf8) (msg :: Text) of
      Nothing -> void . die $ "Failed to parse JSON: " <> toString msg
      Just (se :: SocketEvent) -> do
        case inEnvId se of
          Nothing  -> return ()
          Just id_ -> do
            res <- handleMsg (content se)
            sendTextData
              conn
              (encode $ SocketEventRes { outEnvId = id_, resContent = res })


wsConnect :: Text -> EventHandler -> IO ()
wsConnect wsToken handle = do
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
      Left  e -> void . die . ("Failed to parse URI: " <>) . show $ e
      Right u -> runSecureClient (toString . host $ u)
                                 443
                                 (toString . path $ u)
                                 (wsClient handle)
