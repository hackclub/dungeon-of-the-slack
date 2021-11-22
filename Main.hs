{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Game
import           Slack

import           Control.Concurrent
import           Control.Time
import           System.Environment


handleMsg :: EventHandler
handleMsg msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    SlashCommand t -> return
      $ SlashCommandRes { scrText = "Received: " <> t, scrInChannel = True }
    _ -> die $ "Can't handle event: " <> show msg


-- this is in Main for the sake of modularity
renderGrid :: EntityGrid -> Text
renderGrid = fromString . intercalate "\n" . g2l . fmap renderEntity
 where
  renderEntity e = case e of
    Just _  -> '&'
    Nothing -> '.'

gameLoop :: GameState -> Text -> Text -> IO ()
gameLoop gameState token channelId = do
  delay (5 :: Natural)
    -- meaning depends on type; (5 :: Natural) is 5 seconds

  let (grid, newState) = runState (step renderGrid) gameState
  sendMessage token channelId ("```\n" <> grid <> "\n```")

  gameLoop newState token channelId


main :: IO ()
main = do
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      void . forkIO $ wsConnect wst handleMsg
      channelId <- getChannelId at cn

      gameLoop mkGameState at channelId
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
