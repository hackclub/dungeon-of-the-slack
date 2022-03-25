{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE ViewPatterns        #-}

module Slack
  ( wsConnect
  , EventHandler
  , SocketEventContent(..)
  , SocketEventResContent(..)
  , getChannelID
  , sendMessage
  , editMessage
  , reactToMessage
  , getUserName
  ) where

import           Relude                  hiding ( get
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
  { ok     :: Bool
  , error' :: Maybe Text
  , url    :: Maybe Text
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
  | MemberJoin { channelJoined :: Text, userJoined :: Text }
  | ReactionAdd { reactionName :: Text, reactionMessage :: Text, reactionUser :: Text }
  | SlashCommand { scText :: Text, scUser :: Text }
  | Disconnect
  | Unknown { unknownType :: Text, unknownFull :: Text }
    deriving Show

instance FromJSON SocketEvent where
  parseJSON = withObject "SlackMessage" $ \v -> do
    typeName :: Text <- v .: "type"
    smContent        <-
      (case typeName of
        "hello"      -> return Hello
        "events_api" -> do
          event             <- (v .: "payload") >>= (.: "event")
          eventType :: Text <- event .: "type"
          case eventType of
            "member_joined_channel" -> do
              channel <- event .: "channel"
              user    <- event .: "user"
              return $ MemberJoin channel user
            "reaction_added" -> do
              name <- event .: "reaction"
              msg  <- event .: "item" >>= (.: "ts")
              user <- event .: "user"
              return $ ReactionAdd name msg user
            _ -> return $ Unknown eventType (decodeUtf8 . encodePretty $ v)
        "slash_commands" -> do
          text <- (v .: "payload") >>= (.: "text")
          user <- (v .: "payload") >>= (.: "user_id")
          return $ SlashCommand { scText = text, scUser = user }
        "disconnect" -> return Disconnect
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


type EventHandler m = SocketEventContent -> m SocketEventResContent

wsClient :: EventHandler IO -> Connection -> IO ()
wsClient handleMsg conn = do
  putStrLn "Connected!"
  let msgLoop = do
        msgRaw <- receiveData conn
        let msg = decode msgRaw
        let isDisconnect = case msg of
                             Just (content -> Disconnect) -> True
                             _                            -> False
        unless isDisconnect $ do
          liftIO
            . maybe
                (putTextLn $ "Failed to parse JSON: " <> decodeUtf8 msgRaw)
                (\se ->
                  maybe (return ()) (socketLoop se) . inEnvId $ (se :: SocketEvent)
                )
            $ msg
          msgLoop
  msgLoop
 where
  socketLoop se id_ = do
    res <- handleMsg (content se)
    case res of
      NoRes -> return ()
      _     -> liftIO $ sendTextData
        conn
        (encode SocketEventRes { outEnvId = id_, resContent = res })


wsConnect :: Session -> Text -> EventHandler IO -> IO ()
wsConnect session wsToken handle = forever $ do
  getURLRes <- liftIO $ asJSON =<< S.postWith
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
        . error'
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

getChannelID :: Session -> Text -> Text -> IO Text
getChannelID session token name = do
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
      return . chanId . head' . filter ((== name) . chanName) . channels $ cl
     where
      -- TODO getChannelID should return IO (Maybe Text) and be total
      head' = \case
        c : _ -> c
        []    -> error $ "Couldn't find channel with name: " <> name


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


newtype UserInfo = UserInfo
  { userDisplayName :: Text }
  deriving Show

instance FromJSON UserInfo where
  parseJSON =
    withObject "UserInfo"
      $   (.: "user")
      >=> (.: "profile")
      >=> (.: "display_name")
      >=> return
      .   UserInfo

getUserName :: Session -> Text -> Text -> IO Text
getUserName session token userId = do
  res <- S.getWith
    (  defaults
    &  header "Authorization"
    .~ ["Bearer " <> encodeUtf8 token]
    &  param "user"
    .~ [userId]
    )
    session
    "https://slack.com/api/users.info"
  case eitherDecode (res ^. responseBody) of
    Left  e -> die $ e <> "\n" <> (decodeUtf8 . view responseBody) res
    Right u -> pure . userDisplayName $ u
