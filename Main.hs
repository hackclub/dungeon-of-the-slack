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
import           Utils                          ( m2l )

import           Codec.Picture
import           Codec.Picture.Png
import           Data.FileEmbed

import           Control.Concurrent
import           Control.Time
import           System.Environment


tileset :: [Image PixelRGB8]
tileset = map (fromDynamic . decodePng . snd) $(embedDir "tiles")
 where
  fromDynamic di = case di of
    Right (ImageRGB8 i) -> i
    _                   -> error "???"
      -- we know what the images are, so the type system is more a hindrance here
      -- (there's also a conversion fn though, which i discovered after writing this)

renderGrid :: EntityGrid -> ByteString
renderGrid e = fromLazy . encodePng $ generateImage getPixel 288 288
 where
  getPixel x y = case getEntity of
    Just _ ->
      (\i -> pixelAt i (toTilesetIndex x) (toTilesetIndex y)) $ tileset !! 4
    Nothing -> PixelRGB8 34 35 35 -- other tiles' bg color
   where
    getEntity = toEntityIndex x . toEntityIndex y . m2l $ e
    toEntityIndex i = (!! (i `div` 16))
    toTilesetIndex i = i `div` 2 `mod` 8


handleMsg :: IORef GameState -> EventHandler
handleMsg gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    SlashCommand t -> do
      let cmd = case t of
            "n" -> North
            "e" -> East
            "s" -> South
            "w" -> West
            _   -> Noop
      modifyIORef gsRef $ setCommand cmd
      return
        $ SlashCommandRes { scrText = "Received: " <> t, scrInChannel = True }
    _ -> die $ "Can't handle event: " <> show msg

gameLoop :: IORef GameState -> Text -> Text -> IO ()
gameLoop gsRef token channelId = do
  delay (5 :: Natural)
    -- meaning depends on type; (5 :: Natural) is 5 seconds

  gameState <- readIORef gsRef
  let (img, newState) = runState (step renderGrid) gameState
  writeIORef gsRef newState
  sendMessageFile token channelId img

  gameLoop gsRef token channelId


main :: IO ()
main = do
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      gsRef <- mkGameState Noop >>= newIORef

      void . forkIO $ wsConnect wst (handleMsg gsRef)
      channelId <- getChannelId at cn

      gameLoop gsRef at channelId
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
