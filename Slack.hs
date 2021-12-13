{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slack
  ( wsConnect
  , EventHandler
  , SocketEventContent(..)
  , SocketEventResContent(..)
  , getChannelId
  , sendMessage
  , editMessage
  , reactToMessage
  ) where

import           Prelude                        ( head )
import           Relude                  hiding ( error
                                                , get
                                                , head
                                                , many
                                                )

import           Control.Lens            hiding ( (.=) )
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Text.Megaparsec         hiding ( token )
import           Text.Megaparsec.Char

import           Network.Wreq
import qualified Network.Wreq.Session          as S
import           Network.Wreq.Session           ( Session )

import           Network.WebSockets      hiding ( Message )
import           Wuss


-- WebSocket connection
-----------------------


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
data SocketEventContent =
    Hello
  | ReactionAdd { reactionName :: Text, reactionUser :: Text }
  | SlashCommand { scText :: Text }
  | Unknown { unknownType :: Text, unknownFull :: Text }
    deriving Show

instance FromJSON SocketEvent where
  parseJSON = withObject "SlackMessage" $ \v -> do
    typeName :: Text <- v .: "type"
    smContent        <-
      (case typeName of
        "hello"      -> return Hello
        "events_api" -> do
          event <- (v .: "payload") >>= (.: "event")
          name  <- event .: "reaction"
          user  <- event .: "user"
          return $ ReactionAdd name user
        "slash_commands" -> do
          text <- (v .: "payload") >>= (.: "text")
          return $ SlashCommand { scText = text }
        _ -> do
          type' <- v .: "type"
          let content' = (decodeUtf8 . encodePretty) v
          return $ Unknown type' content'
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
  | BasicRes
  | NoRes

instance ToJSON SocketEventRes where
  toJSON om =
    object
      $ ("envelope_id" .= outEnvId om)
      : (case resContent om of
          SlashCommandRes _ _ ->
            [ "payload" .= object
                [ "text" .= (scrText . resContent) om
                , "response_type"
                  .= (if (scrInChannel . resContent) om
                       then "in_channel"
                       else "ephemeral" :: Text
                     )
                ]
            ]
          _ -> []
        )


type EventHandler = SocketEventContent -> IO SocketEventResContent

wsClient :: EventHandler -> ClientApp ()
wsClient handleMsg conn = do
  putStrLn "Connected!"
  forever $ receiveData conn >>= \msg ->
    liftIO
      . maybe
          (void . die $ "Failed to parse JSON: " <> toString (msg :: Text))
          (\se ->
            maybe (return ()) (socketLoop se) . inEnvId $ (se :: SocketEvent)
          )
      . decode
      . encodeUtf8
      $ msg
 where
  socketLoop se id_ = do
    res <- handleMsg (content se)
    case res of
      NoRes -> return ()
      _     -> sendTextData
        conn
        (encode SocketEventRes { outEnvId = id_, resContent = res })


wsConnect :: Session -> Text -> EventHandler -> IO ()
wsConnect session wsToken handle = do
  getURLRes <- asJSON =<< S.postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 wsToken])
    session
    "https://slack.com/api/apps.connections.open"
    ([] :: [FormParam])

  case url (getURLRes ^. responseBody) of
    Just url' -> case runParser parseURL "" url' of
      Left  e -> void . die . ("Failed to parse URI: " <>) . show $ e
      Right u -> runSecureClient (toString . host $ u)
                                 443
                                 (toString . path $ u)
                                 (wsClient handle)
    Nothing ->
      void
        . die
        . toString
        . ("Failed to get WebSocket URI with: " <>)
        . fromMaybe "(no error)"
        . error
        . (^. responseBody)
        $ getURLRes


-- HTTP requests
----------------


newtype ChannelList = ChannelList { channels :: [Channel] }
instance FromJSON ChannelList where
  parseJSON =
    withObject "ChannelList" $ (return . ChannelList) <=< (.: "channels")

data Channel = Channel
  { chanName :: Text
  , chanId   :: Text
  }
instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \v -> do
    chanName' <- v .: "name"
    chanId'   <- v .: "id"
    return $ Channel { chanName = chanName', chanId = chanId' }

getChannelId :: Session -> Text -> Text -> IO Text
getChannelId session token name = do
  chanListRes <- S.getWith
    (  defaults
    &  header "Authorization"
    .~ ["Bearer " <> encodeUtf8 token]
    &  param "limit"
    .~ ["1000"]
    &  param "types"
    .~ ["public_channel", "private_channel"]
    )
    session
    "https://slack.com/api/conversations.list"
  case eitherDecode (chanListRes ^. responseBody) of
    Left e ->
      die
        $  "Failed to parse JSON for conversation list: "
        <> e
        <> "\n"
        <> decodeUtf8 (chanListRes ^. responseBody)
    Right cl ->
      return . chanId . head . filter ((== name) . chanName) . channels $ cl


newtype SendMsgRes = SendMsgRes
  {
  msgTimestamp :: Maybe Text
  }
  deriving Show
instance FromJSON SendMsgRes where
  parseJSON = withObject "SendMsgRes" $ (.: "ts") >=> return . SendMsgRes

-- returns timestamp
sendMessage :: Session -> Text -> Text -> Text -> IO (Maybe Text)
sendMessage session token channelId text = do
  res <- S.postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token])
    session
    "https://slack.com/api/chat.postMessage"
    (object [("channel", String channelId), ("text", String text)])

  case eitherDecode (res ^. responseBody) of
    Left e -> die $ e <> "\n" <> (decodeUtf8 . view responseBody) res
    Right (SendMsgRes s) -> return s

editMessage :: Session -> Text -> Text -> Text -> Text -> IO ()
editMessage session token channelId timestamp' text = void $ S.postWith
  (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token])
  session
  "https://slack.com/api/chat.update"
  (object
    [ ("channel", String channelId)
    , ("ts"     , String timestamp')
    , ("text"   , String text)
    ]
  )

reactToMessage :: Session -> Text -> Text -> Text -> Text -> IO ()
reactToMessage session token channelId timestamp' emoji = void $ S.post
  session
  "https://slack.com/api/reactions.add"
  [ "token" := token
  , "channel" := channelId
  , "timestamp" := timestamp'
  , "name" := emoji
  ]
