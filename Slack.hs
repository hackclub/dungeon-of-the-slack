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
  -- , sendMessage
  , sendMessageFile
  , editMessageFile
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
import           Network.Wreq

import           Text.Megaparsec         hiding ( token )
import           Text.Megaparsec.Char

import           Data.ByteString.Base64         ( encodeBase64 )
import           Network.WebSockets
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
  | ReactionAdd {reactionName :: Text, reactionUser :: Text}
  | SlashCommand {scText :: Text}
  | Unknown {unknownType :: Text, unknownFull :: Text}
    deriving Show

instance FromJSON SocketEvent where
  parseJSON = withObject "SlackMessage" $ \v -> do
    typeName :: Text <- v .: "type"
    smContent        <-
      (case typeName of
        "hello"      -> return Hello
        "events_api" -> do
          name <- (v .: "payload") >>= (.: "event") >>= (.: "reaction")
          user <- (v .: "payload") >>= (.: "event") >>= (.: "user")
          return $ ReactionAdd name user
        "slash_commands" -> do
          text <- (v .: "payload") >>= (.: "text")
          return $ SlashCommand { scText = text }
        _ -> do
          type_ <- v .: "type"
          let fullContent = (decodeUtf8 . encodePretty) v
          return $ Unknown type_ fullContent
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
  } | BasicRes | NoRes

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
  forever $ receiveData conn >>= \msg -> do
    liftIO $ case (decode . encodeUtf8) (msg :: Text) of
      Nothing -> void . die $ "Failed to parse JSON: " <> toString msg
      Just (se :: SocketEvent) -> do
        case inEnvId se of
          Nothing  -> return ()
          Just id_ -> do
            res <- handleMsg (content se)
            case res of
              NoRes -> return ()
              _     -> sendTextData
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


-- HTTP requests
----------------


newtype ChannelList = ChannelList { channels :: [Channel] }
instance FromJSON ChannelList where
  parseJSON =
    withObject "ChannelList" $ (return . ChannelList) <=< (.: "channels")

data SendFileRes = SendFileRes
  { fileID    :: Text
  , timestamp :: Maybe Text
  }
  deriving Show
instance FromJSON SendFileRes where
  parseJSON = withObject
    "SendFileRes"
    (\o -> do
      channelShares <- (o .: "file") >>= (.: "channels")
      groupShares   <- (o .: "file") >>= (.: "groups")
      let msg = head (channelShares ++ groupShares)
      fileID'       <- o .: "file" >>= (.: "id")
      shareIfExists <- o .: "file" >>= (.: "shares") >>= (.:? "private")
      timestamp'    <- case shareIfExists of
        Just s  -> s .: msg >>= ((.: "ts") . head)
        Nothing -> return Nothing
        -- >>= (.: msg)
        -- >>= ((.: "ts") . head)
      return $ SendFileRes fileID' timestamp'
    )

newtype ImgBBRes = ImgBBRes { imgbbURL :: Text }
instance FromJSON ImgBBRes where
  parseJSON = withObject
    "ImgBBRes"
    (\o -> o .: "data" >>= (.: "url") >>= return . ImgBBRes)

data Channel = Channel
  { chanName :: Text
  , chanId   :: Text
  }
instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \v -> do
    chanName' <- v .: "name"
    chanId'   <- v .: "id"
    return $ Channel { chanName = chanName', chanId = chanId' }

getChannelId :: Text -> Text -> IO Text
getChannelId token name = do
  chanListRes <- getWith
    (  defaults
    &  header "Authorization"
    .~ ["Bearer " <> encodeUtf8 token]
    &  param "limit"
    .~ ["1000"]
    &  param "types"
    .~ ["public_channel", "private_channel"]
    )
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


-- sendMessage :: Text -> Text -> Text -> IO ()
-- sendMessage token channelId msgContent = void $ post
--   "https://slack.com/api/chat.postMessage"
--   ["token" := token, "channel" := channelId, "text" := msgContent]

-- returns timestamp
sendMessageFile :: Text -> Text -> ByteString -> IO (Maybe Text)
sendMessageFile token channelId msgContent = do
  res <- post
    "https://slack.com/api/files.upload"
    ["token" := token, "channels" := channelId, "content" := msgContent]
  print (decode (res ^. responseBody) :: Maybe Value)
  case eitherDecode (res ^. responseBody) of
    Left e -> die (e <> "\n" <> (decodeUtf8 . view responseBody) res)
    Right (SendFileRes _ s) -> return s

editMessageFile :: Text -> Text -> Text -> Text -> ByteString -> IO ()
editMessageFile token channelId imgbb timestamp' msgContent = do
  imgBBRes <- post "https://api.imgbb.com/1/upload"
                   ["key" := imgbb, "image" := encodeBase64 msgContent]
  url' <- case eitherDecode (imgBBRes ^. responseBody) of
    Left  e               -> die e
    Right (ImgBBRes url') -> return url'

  void $ postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token])
    "https://slack.com/api/chat.update"
    (object
      [ ("channel", String channelId)
      , ("ts"     , String timestamp')
      , ( "blocks"
        , Array
          (fromList
            [ object
                [ ("type"     , "image")
                , ("image_url", String url')
                , ("alt_text" , "a fearsome dungeon")
                ]
            ]
          )
        )
      , ("file_ids", Array empty)
      , ("text"    , String "...")
      ]
    )

reactToMessage :: Text -> Text -> Text -> Text -> IO ()
reactToMessage token channelId timestamp' emoji = void $ post
  "https://slack.com/api/reactions.add"
  [ "token" := token
  , "channel" := channelId
  , "timestamp" := timestamp'
  , "name" := emoji
  ]
