{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Control.Concurrent
import           Control.Time
import           Slack
import           System.Environment


handleMsg :: EventHandler
handleMsg msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    SlashCommand t -> return
      $ SlashCommandRes { scrText = "Received: " <> t, scrInChannel = True }
    _ -> die $ "Can't handle event: " <> show msg


gameLoop :: Text -> Text -> IO ()
gameLoop token channelId = do
  delay (5 :: Natural)
    -- meaning depends on type; (5 :: Natural) is 5 seconds
  sendMessage token channelId "Five seconds have passed"
  gameLoop token channelId


main :: IO ()
main = do
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      void . forkIO $ wsConnect wst handleMsg
      channelId <- getChannelId at cn
      gameLoop at channelId
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
