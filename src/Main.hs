{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Relude

import           Game
import           Slack
import           Utils

import           Control.Concurrent.Chan
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )
import           Network.Wreq.Session           ( Session )
import qualified Network.Wreq.Session          as S
import           System.Environment             ( lookupEnv )
import           UnliftIO.Concurrent            ( forkIO )


data Context = Context
  { ctxSession   :: Session
  , ctxAPIToken  :: Text
  , ctxWSToken   :: Text
  , ctxChannelID :: Text
  }

type AppM = ReaderT Context RogueM


-- user id hardcoded for convenience. sorry!
rogueUserId :: Text
rogueUserId = "U02MTTW1XND"

addReacts :: Text -> AppM ()
addReacts timestamp = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask
  void . liftIO . forkIO $ mapM_
    (reactToMessage session token channelID timestamp)
    [ "tw_arrow_up"
    , "tw_arrow_right"
    , "tw_arrow_down"
    , "tw_arrow_left"
    , "tw_tea"
    ]

fromReact :: Text -> Command
fromReact = \case
  "tw_arrow_up"    -> Move (0, -1)
  "tw_arrow_right" -> Move (1, 0)
  "tw_arrow_down"  -> Move (0, 1)
  "tw_arrow_left"  -> Move (-1, 0)
  "tw_tea"         -> Drink
  _                -> Noop

renderGrid :: EntityGrid -> RogueM Text
renderGrid es =
  mapM fromCoord coordMatrix <&> fromString . concat . intercalate ["\n"] . m2l
 where
  coordMatrix = (l2m . chunksOf matrixSize)
    [ (x, y) | y <- [0 .. matrixSize - 1], x <- [0 .. matrixSize - 1] ]

  fromCoord (x, y) = do
    vertical' <- vertical (x, y)
    mapM represent (mget x y es) <&> fromEntityRepr vertical'

  fromEntityRepr vertical' = \case
    Just WallTile ->
      if vertical' then ":rogue__wall_vert:" else ":rogue__wall_horiz:"
    Just DoorTile ->
      if vertical' then ":rogue__door_vert:" else ":rogue__door_horiz:"
    Just EvilTile         -> ":rogue__rat:"
    Just FireTile         -> ":rogue__fire:"
    Just PotionTile       -> ":rogue__potion:"
    Just (PortalTile In ) -> ":rogue__portal_in:"
    Just (PortalTile Out) -> ":rogue__portal_out:"
    Just StaircaseTile    -> ":rogue__staircase:"
    Just PlayerTile       -> ":rogue__player:"
    Just ErrorTile        -> ":rogue__default:"
    Nothing               -> ":rogue__blank:"

  isWallOrDoor' e = represent e <&> ((== WallTile) ||$ (== DoorTile))
  isWallOrDoor x' y' = maybe (pure False) isWallOrDoor' $ mget x' y' es
  safeInc a = if a < matrixSize - 1 then a + 1 else a
  safeDec a = if a > 0 then a - 1 else a
  vertical (x, y) = do
    l <- sequence
      [ isWallOrDoor x           (safeDec y)
      , isWallOrDoor x           (safeInc y)
      , isWallOrDoor (safeDec x) y
      , isWallOrDoor (safeInc x) y
      ]
    let [a, b, c, d] = l
    return $ a && b && not (c && d)

stepAndSend :: Maybe Text -> Command -> AppM Text
stepAndSend edit cmd = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask

  (renderedGrid, message) <- lift
    $ step cmd renderGrid (pure . unlines . unMessage)
  let text = message <> "\n" <> renderedGrid

  case edit of
    Nothing -> do
      timestamp <-
        liftIO $ sendMessage session token channelID text <&> fromJust
      addReacts timestamp
      return timestamp
    Just timestamp -> do
      void . liftIO . forkIO $ editMessage session
                                           token
                                           channelID
                                           timestamp
                                           text
      return timestamp

handleMsg :: Text -> Chan (Maybe Text, Command) -> EventHandler IO
handleMsg timestamp channel msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> do
      unless (u == rogueUserId) . void $ writeChan
        channel
        (Just timestamp, fromReact e)
      return BasicRes

    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes

app :: AppM ()
app = do
  context   <- ask
  timestamp <- stepAndSend Nothing Noop
  channel   <- liftIO newChan
  void . liftIO . forkIO $ wsConnect (ctxSession context)
                                     (ctxWSToken context)
                                     (handleMsg timestamp channel)
  forever $ do
    (timestamp', command) <- liftIO $ readChan channel
    stepAndSend timestamp' command

main :: IO ()
main = do
  let envVarNames = ["SLACK_API_TOKEN", "SLACK_WS_TOKEN", "RL_CHANNEL_NAME"]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn] -> do
      session   <- S.newSession
      channelID <- getChannelID session at cn
      let context = Context { ctxSession   = session
                            , ctxAPIToken  = at
                            , ctxWSToken   = wst
                            , ctxChannelID = channelID
                            }
      runRogue $ runReaderT app context

    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
