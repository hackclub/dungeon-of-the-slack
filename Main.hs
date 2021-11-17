{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Slack
import           System.Environment


handleMsg :: EventHandler
handleMsg msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    SlashCommand ->
      return $ SlashCommandRes { scrText = "Received!", scrInChannel = True }
    _ -> die $ "Can't handle event: " <> show msg


main :: IO ()
main = do
  -- apiToken <- lookupEnv "SLACK_API_TOKEN"
  wsToken <- lookupEnv "SLACK_WS_TOKEN"
  case wsToken of
    Just wst -> wsConnect (fromString wst) handleMsg
    Nothing  -> putStrLn "Can't find SLACK_WS_TOKEN environment variable"
