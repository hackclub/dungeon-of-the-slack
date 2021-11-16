{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Slack
import           System.Environment


main :: IO ()
main = do
  apiToken <- lookupEnv "SLACK_API_TOKEN"
  wsToken  <- lookupEnv "SLACK_WS_TOKEN"
  case (apiToken, wsToken) of
    (Just at, Just wst) -> connect (fromString at) (fromString wst)
    _                   -> putStrLn
      "Can't find SLACK_API_TOKEN and/or SLACK_WS_TOKEN environment variables"
