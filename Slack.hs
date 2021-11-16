{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack
  ( connect
  ) where

import           Relude                  hiding ( error )

import           Control.Lens
import           Data.Aeson                     ( FromJSON )
import           Network.Wreq


data GetConnResponse = GetConnResponse
  { ok    :: Bool
  , error :: Maybe String
  , url   :: Maybe String
  }
  deriving Generic
instance FromJSON GetConnResponse


connect :: Text -> Text -> IO ()
connect _ wsToken = do
  getConnRes <- asJSON =<< postWith
    (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 wsToken])
    "https://slack.com/api/apps.connections.open"
    ([] :: [FormParam])

  case url (getConnRes ^. responseBody) of
    Nothing ->
      void
        . die
        . ("Failed to get WebSocket URI with: " <>)
        . fromMaybe "(no error)"
        . error
        . (^. responseBody)
        $ getConnRes
    Just url' -> connect' url'

  return ()
 where
  connect' url = do
    -- TODO
    undefined
