{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- `handleMsg`, `renderGrid`, `gameLoop` etc. are in Main for the sake of
-- modularity

module Main
  ( main
  ) where

import           Prelude                        ( (!!) )
import           Relude

import           Game
import           Slack
import           Utils                          ( l2m
                                                , m2l
                                                , matrixSize
                                                , mget
                                                )

import           Codec.Picture
import           Data.FileEmbed

import           Data.Aeson
import           Data.ByteString.Base64
import           Network.Wreq
import qualified Network.Wreq.Session          as S
import           Network.Wreq.Session           ( Session )

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Async       ( mapConcurrently_ )
import           Control.Lens                   ( (^.)
                                                , view
                                                )
import           Data.Default
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )
import           System.Environment


-- user id hardcoded for convenience. sorry!
ourUserId :: Text
ourUserId = "U02MTTW1XND"


-- image mode
-------------


tileset :: [Image PixelRGB8]
tileset = map (fromDynamic . decodePng . snd) $(embedDir "tiles")
  where fromDynamic di = convertRGB8 . either (error . fromString) id $ di

pixelSize :: Int
pixelSize = 3

tileSize :: Int
tileSize = pixelSize * 8

renderGridImg :: EntityGrid -> ByteString
renderGridImg e = fromLazy . encodePng $ generateImage
  getPixel
  (matrixSize * tileSize)
  (matrixSize * tileSize)
 where
  getPixel x y = case getEntity of
    Just e' ->
      (\i -> pixelAt i (toTilesetIndex x) (toTilesetIndex y))
        $  tileset
        !! (fromEntityRepr . represent $ e')
    Nothing -> PixelRGB8 34 35 35 -- other tiles' bg color
   where
    wallExists x' y' =
      maybe False ((\e' -> isWall e' || isDoor e') . view components)
        . toEntityIndex x'
        . toEntityIndex y'
        . m2l
        $ e
    wallAbove = wallExists x (if y > tileSize then y - tileSize else 0)
    wallBelow = wallExists
      x
      (if y < (matrixSize * tileSize - tileSize) then y + tileSize else y)
    wallToLeft  = wallExists (if x > tileSize then x - tileSize else 0) y
    wallToRight = wallExists
      (if x < (matrixSize * tileSize - tileSize) then x + tileSize else 0)
      y
    vertical  = wallAbove && wallBelow && not (wallToLeft && wallToRight)

    getEntity = toEntityIndex x . toEntityIndex y . m2l $ e
    fromEntityRepr t = case t of
      DefaultTile -> 96 -- closest i could find to "missing texture" so to speak
      PlayerTile  -> 4
      WallTile    -> if vertical then 148 else 1
      DoorTile    -> if vertical then 149 else 2
      EvilTile    -> 22
      PotionTile  -> 135
    toEntityIndex  = flip (!!) . flip div tileSize
    toTilesetIndex = flip mod 8 . flip div pixelSize


newtype ImgBBRes = ImgBBRes Text
instance FromJSON ImgBBRes where
  parseJSON = withObject
    "ImgBBRes"
    (\o -> o .: "data" >>= (.: "url") >>= return . ImgBBRes)

stepAndSendImg
  :: Session
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Command
  -> GameState
  -> IO (Text, GameState)
stepAndSendImg session token channelId imgbb edit cmd gameState = do
  let (img, newState) =
        runState (step renderGridImg) $ setCommand cmd gameState
  imgBBRes <- S.post session
                     "https://api.imgbb.com/1/upload"
                     ["key" := imgbb, "image" := encodeBase64 img]
  imgURL <- case eitherDecode (imgBBRes ^. responseBody) of
    Left  e               -> die e
    Right (ImgBBRes url') -> return url'

  case edit of
    Nothing -> do
      timestamp <-
        sendMessage
          session
          token
          channelId
          (Right $ def { blkText     = Just "init"
                       , blkImageURL = Just imgURL
                       , blkImageAlt = Just "a fearsome dungeon"
                       }
          )
        >>= return
        .   fromJust
      mapM_
        (reactToMessage session token channelId timestamp)
        [ "tw_arrow_up"
        , "tw_arrow_right"
        , "tw_arrow_down"
        , "tw_arrow_left"
        , "tw_tea"
        ]
      return (timestamp, newState)
    Just timestamp -> do
      (void . forkIO) $ editMessage
        session
        token
        channelId
        timestamp
        (Right $ def { blkText     = Just "updated"
                     , blkImageURL = Just imgURL
                     , blkImageAlt = Just "a fearsome dungeon"
                     }
        )
      return (timestamp, newState)

handleMsgImg
  :: Session -> Text -> Text -> Text -> Text -> IORef GameState -> EventHandler
handleMsgImg session token channelId imgbb timestamp gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> if u == ourUserId
      then return BasicRes
      else do
        let cmd = case e of
              "tw_arrow_up"    -> North
              "tw_arrow_right" -> East
              "tw_arrow_down"  -> South
              "tw_arrow_left"  -> West
              "tw_tea"         -> Drink
              _                -> Noop
        (_, newState) <-
          readIORef gsRef
            >>= stepAndSendImg session
                               token
                               channelId
                               imgbb
                               (Just timestamp)
                               cmd
        writeIORef gsRef newState

        return BasicRes
    Unknown _ f -> do
      putTextLn ("Ah shit\n" <> f)
      return NoRes
    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes


-- emoji mode
-------------


renderGridEmoji :: EntityGrid -> Text
-- renderGridEmoji = fromString . concat . intercalate ["\n"] . m2l . fmap
--   (fromEntityRepr . fmap represent)
renderGridEmoji es =
  fromString . concat . intercalate ["\n"] . m2l . fmap fromCoord $ coordMatrix
 where
  coordMatrix = (l2m . chunksOf 14) [ (x, y) | y <- [0 .. 13], x <- [0 .. 13] ]
  fromCoord (x, y) = (fromEntityRepr vertical . fmap represent) (mget x y es)
   where
    isWallOrDoor x' y' =
      maybe False ((\e -> isWall e || isDoor e) . view components)
        $ mget x' y' es
    safeInc a = if a < 13 then a + 1 else a
    safeDec a = if a > 0 then a - 1 else a
    vertical =
      isWallOrDoor x (safeDec y) && isWallOrDoor x (safeInc y) && not
        (isWallOrDoor (safeDec x) y && isWallOrDoor (safeInc x) y)
  fromEntityRepr vertical = \case
    Just DefaultTile -> ":rogue__default:"
    Just PlayerTile  -> ":rogue__player:"
    Just WallTile ->
      if vertical then ":rogue__wall_vert:" else ":rogue__wall_horiz:"
    Just DoorTile ->
      if vertical then ":rogue__door_vert:" else ":rogue__door_horiz:"
    Just EvilTile   -> ":rogue__rat:"
    Just PotionTile -> ":rogue__potion:"
    Nothing         -> ":rogue__blank:"

stepAndSendEmoji
  :: Session
  -> Text
  -> Text
  -> Maybe Text
  -> Command
  -> GameState
  -> IO (Text, GameState)
stepAndSendEmoji session token channelId edit cmd gameState = do
  let (text, newState) =
        runState (step renderGridEmoji) $ setCommand cmd gameState
  case edit of
    Nothing -> do
      timestamp <-
        sendMessage session token channelId (Left text) >>= return . fromJust
      void . forkIO $ mapConcurrently_
        (reactToMessage session token channelId timestamp)
        [ "tw_arrow_up"
        , "tw_arrow_right"
        , "tw_arrow_down"
        , "tw_arrow_left"
        , "tw_tea"
        ]
      return (timestamp, newState)
    Just timestamp -> do
      void . forkIO $ editMessage session token channelId timestamp (Left text)
      return (timestamp, newState)

-- this is mostly copy-pasted from image mode and probably shouldn't be!
handleMsgEmoji
  :: Session -> Text -> Text -> Text -> IORef GameState -> EventHandler
handleMsgEmoji session token channelId timestamp gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> if u == ourUserId
      then return BasicRes
      else do
        let cmd = case e of
              "tw_arrow_up"    -> North
              "tw_arrow_right" -> East
              "tw_arrow_down"  -> South
              "tw_arrow_left"  -> West
              "tw_tea"         -> Drink
              _                -> Noop
        (_, newState) <-
          readIORef gsRef
            >>= stepAndSendEmoji session token channelId (Just timestamp) cmd
        writeIORef gsRef newState

        return BasicRes
    Unknown _ f -> do
      putTextLn ("Ah shit\n" <> f)
      return NoRes
    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes


-- main
-------


main :: IO ()
main = do
  s         <- S.newSession

  envVarMod <- lookupEnv "RL_DELIVERY_MODE" >>= \case
    Just "emoji" -> return []
    Just "image" -> return ["IMGBB_API_KEY"]
    _ -> die "Please set RL_DELIVERY_MODE to either 'emoji' or 'image'"

  let envVarNames =
        ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"] ++ envVarMod
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of

    -- image mode
    [Just at, Just wst, Just cn, Just imgbb] -> do
      channelId              <- getChannelId s at cn
      (timestamp, gameState) <-
        mkGameState Noop >>= stepAndSendImg s at channelId imgbb Nothing Noop
      gsRef <- newIORef gameState
      wsConnect s wst (handleMsgImg s at channelId imgbb timestamp gsRef)

    -- emoji mode
    [Just at, Just wst, Just cn] -> do
      channelId              <- getChannelId s at cn
      (timestamp, gameState) <-
        mkGameState Noop >>= stepAndSendEmoji s at channelId Nothing Noop
      gsRef <- newIORef gameState
      wsConnect s wst (handleMsgEmoji s at channelId timestamp gsRef)

    -- oh no
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
