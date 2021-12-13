{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Game
import           Slack
import           Utils                          ( l2m
                                                , m2l
                                                , matrixSize
                                                , mget
                                                )

import qualified Network.Wreq.Session          as S
import           Network.Wreq.Session           ( Session )

import           Control.Concurrent             ( forkIO )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )
import           System.Environment             ( lookupEnv )


-- user id hardcoded for convenience. sorry!
rogueUserId :: Text
rogueUserId = "U02MTTW1XND"

addReacts :: Session -> Text -> Text -> Text -> IO ()
addReacts session token channelId timestamp = void . forkIO $ mapM_
  (reactToMessage session token channelId timestamp)
  ["tw_arrow_up", "tw_arrow_right", "tw_arrow_down", "tw_arrow_left", "tw_tea"]

fromReact :: Text -> Command
fromReact = \case
  "tw_arrow_up"    -> North
  "tw_arrow_right" -> East
  "tw_arrow_down"  -> South
  "tw_arrow_left"  -> West
  "tw_tea"         -> Drink
  _                -> Noop

renderGrid :: EntityGrid -> Text
renderGrid es =
  fromString . concat . intercalate ["\n"] . m2l . fmap fromCoord $ coordMatrix
 where
  coordMatrix = (l2m . chunksOf matrixSize)
    [ (x, y) | y <- [0 .. matrixSize - 1], x <- [0 .. matrixSize - 1] ]
  fromCoord (x, y) = (fromEntityRepr vertical . fmap represent) (mget x y es)
   where
    isWallOrDoor x' y' =
      maybe False (\e -> isWall e || isDoor e) $ mget x' y' es
    safeInc a = if a < matrixSize - 1 then a + 1 else a
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

stepAndSend
  :: Session
  -> Text
  -> Text
  -> Maybe Text
  -> Command
  -> GameState
  -> IO (Text, GameState)
stepAndSend session token channelId edit cmd gameState = do
  let (renderedGrid, newState) =
        runState (step renderGrid) $ setCommand cmd gameState
      text = (unlines . getMessage $ gameState) <> "\n" <> renderedGrid
  case edit of
    Nothing -> do
      timestamp <- sendMessage session token channelId text <&> fromJust
      addReacts session token channelId timestamp
      return (timestamp, newState)
    Just timestamp -> do
      void . forkIO $ editMessage session token channelId timestamp text
      return (timestamp, newState)

handleMsg :: Session -> Text -> Text -> Text -> IORef GameState -> EventHandler
handleMsg session token channelId timestamp gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> if u == rogueUserId
      then return BasicRes
      else do
        let cmd = fromReact e
        (_, newState) <-
          readIORef gsRef
            >>= stepAndSend session token channelId (Just timestamp) cmd
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
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      session                <- S.newSession
      channelId              <- getChannelId session at cn
      (timestamp, gameState) <-
        mkGameState Noop >>= stepAndSend session at channelId Nothing Noop
      newIORef gameState
        >>= wsConnect session wst
        .   handleMsg session at channelId timestamp

    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
