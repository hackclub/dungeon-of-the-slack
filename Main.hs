{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- `handleMsg`, `renderGrid`, `gameLoop` etc. are in Main for the sake of
-- modularity

module Main
  ( main
  ) where

import           Prelude                        ( (!!)
                                                , maximum
                                                )
import           Relude

import           Game
import           Slack
import           Utils                          ( m2l )

import           Codec.Picture
import           Data.FileEmbed

import           Control.Concurrent
import           Control.Time
import qualified Data.Map                      as Map
import           System.Environment


tileset :: [Image PixelRGB8]
tileset = map (fromDynamic . decodePng . snd) $(embedDir "tiles")
  where fromDynamic di = convertRGB8 . either (error . fromString) id $ di

renderGrid :: EntityGrid -> ByteString
renderGrid e = fromLazy . encodePng $ generateImage getPixel 384 384
 where
  getPixel x y = case getEntity of
    Just e' ->
      (\i -> pixelAt i (toTilesetIndex x) (toTilesetIndex y))
        $  tileset
        !! (fromEntityRepr . represent $ e')
    Nothing -> PixelRGB8 34 35 35 -- other tiles' bg color
   where
    getEntity = toEntityIndex x . toEntityIndex y . m2l $ e
    fromEntityRepr t = case t of
      EmptyTile  -> 17
      PlayerTile -> 4
      WallTile   -> 1
    toEntityIndex  = flip (!!) . flip div 16
    toTilesetIndex = flip mod 8 . flip div 2


type VoteCounter = Map Command Int

handleMsg :: IORef VoteCounter -> EventHandler
handleMsg vcRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    SlashCommand t -> do
      let cmd = case t of
            "north" -> North
            "n"     -> North
            "east"  -> East
            "e"     -> East
            "south" -> South
            "s"     -> South
            "west"  -> West
            "w"     -> West
            _       -> Noop

      modifyIORef vcRef $ Map.insertWith (+) cmd 1
      return $ SlashCommandRes { scrText      = "Received: " <> show cmd
                               , scrInChannel = True
                               }
    _ -> die $ "Can't handle event: " <> show msg

gameLoop :: IORef VoteCounter -> GameState -> Text -> Text -> IO ()
gameLoop vcRef gameState token channelId = do
  delay (5 :: Natural)
    -- meaning depends on type; (5 :: Natural) is 5 seconds

  voteCounter <- readIORef vcRef
  let topVote = if null voteCounter
        then Nothing
        else
          listToMaybe
          . Map.keys
          . Map.filter (== (maximum . Map.elems $ voteCounter))
          $ voteCounter
  writeIORef vcRef mempty

  newState <- case topVote of
    Just cmd -> do
      let (img, newState) =
            runState (step renderGrid) $ setCommand cmd gameState
      sendMessageFile token channelId img
      return newState
    Nothing -> do
      return gameState

  gameLoop vcRef newState token channelId


main :: IO ()
main = do
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      vsRef <- newIORef mempty
      void . forkIO $ wsConnect wst (handleMsg vsRef)
      channelId <- getChannelId at cn

      gameState <- mkGameState Noop
      gameLoop vsRef gameState at channelId
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
