{-# LANGUAGE BlockArguments    #-}
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
import           Utils                          ( m2l
                                                , matrixSize
                                                )

import           Codec.Picture
import           Data.FileEmbed

import           Control.Lens                   ( view )
import           Data.Maybe                     ( fromJust )
import           System.Environment


tileset :: [Image PixelRGB8]
tileset = map (fromDynamic . decodePng . snd) $(embedDir "tiles")
  where fromDynamic di = convertRGB8 . either (error . fromString) id $ di

pixelSize :: Int
pixelSize = 3

tileSize :: Int
tileSize = pixelSize * 8

renderGrid :: EntityGrid -> ByteString
renderGrid e = fromLazy . encodePng $ generateImage getPixel
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


-- type VoteCounter = Map Command Int

stepAndSend
  :: Text
  -> Text
  -> Text
  -> Maybe Text
  -> Command
  -> GameState
  -> IO (Text, GameState)
stepAndSend token channelId imgbb edit cmd gameState = do
  let (img, newState) = runState (step renderGrid) $ setCommand cmd gameState
  case edit of
    Nothing -> do
      timestamp <- sendMessageFile token channelId img >>= return . fromJust
      mapM_
        (reactToMessage token channelId timestamp)
        [ "tw_arrow_up"
        , "tw_arrow_right"
        , "tw_arrow_down"
        , "tw_arrow_left"
        , "tw_tea"
        ]
      return (timestamp, newState)
    Just timestamp -> do
      editMessageFile token channelId imgbb timestamp img
      return (timestamp, newState)

handleMsg :: Text -> Text -> Text -> Text -> IORef GameState -> EventHandler
handleMsg token channelId imgbb timestamp gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> if u == "U02MTTW1XND" -- user id hardcoded for convenience. sorry!
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
            >>= stepAndSend token channelId imgbb (Just timestamp) cmd
        writeIORef gsRef newState

        return BasicRes
    Unknown _ f -> do
      putTextLn ("Ah shit\n" <> f)
      return NoRes
    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes


main :: IO ()
main = do
  let envVarNames =
        [ "SLACK_API_TOKEN"
        , "SLACK_WS_TOKEN"
        , "RL_CHANNEL_NAME"
        , "IMGBB_API_KEY"
        ]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn, Just imgbb] -> do
      channelId              <- getChannelId at cn
      (timestamp, gameState) <-
        mkGameState Noop >>= stepAndSend at channelId imgbb Nothing Noop

      gsRef <- newIORef gameState
      wsConnect wst (handleMsg at channelId imgbb timestamp gsRef)

    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
