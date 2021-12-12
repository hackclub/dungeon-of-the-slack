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
  -- , sendMessageFile
  , editMessage
  -- , editMessageFile
  , MessageBlocks(..)
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
import           Data.Default
import           Network.Wreq
import qualified Network.Wreq.Session          as S
import           Network.Wreq.Session           ( Session )

import           Text.Megaparsec         hiding ( token )
import           Text.Megaparsec.Char

-- import           Data.ByteString.Base64         ( encodeBase64 )
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


wsConnect :: Session -> Text -> EventHandler -> IO ()
wsConnect session wsToken handle = do
  getURLRes <- asJSON =<< S.postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 wsToken])
    session
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

-- utilities for constructing json

array :: [Value] -> Value
array = Array . fromList


newtype ChannelList = ChannelList { channels :: [Channel] }
instance FromJSON ChannelList where
  parseJSON =
    withObject "ChannelList" $ (return . ChannelList) <=< (.: "channels")

data MessageBlocks = MessageBlocks
  { blkText     :: Maybe Text
  , blkImageURL :: Maybe Text
  , blkImageAlt :: Maybe Text
  , blkActionID :: Maybe Text
  , blkButtons  :: [Text]
  }
  deriving Generic
instance ToJSON MessageBlocks where
  toJSON (MessageBlocks bt biu bia baid bbs) = array
    (textJSON <> imageJSON <> btnsJSON)   where
    textJSON = case bt of
      Just t ->
        [ object
            [ ("type", "section")
            , ("text", object [("type", "mrkdwn"), ("text", String t)])
            ]
        ]
      Nothing -> []
    imageJSON = case (biu, bia) of
      (Just iu, Just ia) ->
        [ object
            [ ("type"     , "image")
            , ("image_url", String iu)
            , ("alt_text" , String ia)
            ]
        ]
      _ -> []
    btnsJSON = case baid of
      Just aid ->
        [ object
            [ ("type"    , "actions")
            , ("block_id", String aid)
            , ("elements", array (map toButtonJSON bbs))
            ]
        ]
      Nothing -> []
    toButtonJSON b = object
      [ ("type", "button")
      , ("text", object [("type", "mrkdwn"), ("text", String b)])
      ]
instance Default MessageBlocks

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
      return $ SendFileRes fileID' timestamp'
    )

newtype SendMsgRes = SendMsgRes
  {
  msgTimestamp :: Maybe Text
  }
  deriving Show
instance FromJSON SendMsgRes where
  parseJSON = withObject "SendMsgRes" ((.: "ts") >=> return . SendMsgRes)

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


-- returns timestamp
sendMessage
  :: Session -> Text -> Text -> Either Text MessageBlocks -> IO (Maybe Text)
sendMessage session token channelId content' = do
  res <- S.postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token])
    session
    "https://slack.com/api/chat.postMessage"
    (object [("channel", String channelId), contentKV])

  case eitherDecode (res ^. responseBody) of
    Left e -> die (e <> "\n" <> (decodeUtf8 . view responseBody) res)
    Right (SendMsgRes s) -> return s

 where
  contentKV = case content' of
    Left  t  -> ("text", String t)
    Right mb -> ("blocks", toJSON mb)

editMessage
  :: Session -> Text -> Text -> Text -> Either Text MessageBlocks -> IO ()
editMessage session token channelId timestamp' content' = void $ S.postWith
  (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token])
  session
  "https://slack.com/api/chat.update"
  (object [("channel", String channelId), ("ts", String timestamp'), contentKV])
 where
  contentKV = case content' of
    Left  t  -> ("text", String t)
    Right mb -> ("blocks", toJSON mb)

reactToMessage :: Session -> Text -> Text -> Text -> Text -> IO ()
reactToMessage session token channelId timestamp' emoji = void $ S.post
  session
  "https://slack.com/api/reactions.add"
  [ "token" := token
  , "channel" := channelId
  , "timestamp" := timestamp'
  , "name" := emoji
  ]
