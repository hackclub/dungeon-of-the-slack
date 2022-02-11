{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main
  ( main
  ) where

import           Relude

import           Game
import           Slack
import           Utils

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decodeStrict
                                                , encodeFile
                                                )
import           System.Directory               ( doesFileExist )

import           Network.Wreq.Session           ( Session )
import qualified Network.Wreq.Session          as S

import           UnliftIO.Async
import           UnliftIO.Concurrent            ( threadDelay )

import           Control.Concurrent.Chan
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )
import           Data.Time                      ( getCurrentTime )
import           System.Environment             ( lookupEnv )


data Context = Context
  { ctxSession         :: Session
  , ctxAPIToken        :: Text
  , ctxWSToken         :: Text
  , ctxChannelID       :: Text
  , ctxLeaderboardFile :: Text
  }

type AppM = ReaderT Context RogueM


deriving instance Generic LeaderboardEntry
deriving instance Generic Leaderboard

instance FromJSON LeaderboardEntry
instance ToJSON LeaderboardEntry

instance FromJSON Leaderboard
instance ToJSON Leaderboard


-- user id hardcoded for convenience. sorry!
rogueUserId :: Text
rogueUserId = "U02MTTW1XND"

addReacts :: Text -> AppM ()
addReacts timestamp = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask
  void . liftIO . async $ mapM_
    (reactToMessage session token channelID timestamp)
    [ "tw_hourglass"
    , "tw_arrow_up"
    , "tw_arrow_right"
    , "tw_arrow_down"
    , "tw_arrow_left"
    , "tw_tea"
    , "tw_skull"
    ]

fromReact :: Text -> Command
fromReact = \case
  "tw_hourglass"   -> Noop
  "tw_arrow_up"    -> Move (0, -1)
  "tw_arrow_right" -> Move (1, 0)
  "tw_arrow_down"  -> Move (0, 1)
  "tw_arrow_left"  -> Move (-1, 0)
  "tw_tea"         -> Drink
  "tw_skull"       -> Die
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
  isSomething x' y' = pure . isJust $ mget x' y' es
  safeInc a = if a < matrixSize - 1 then a + 1 else a
  safeDec a = if a > 0 then a - 1 else a
  vertical (x, y) = do
    l <- sequence
      [ isSomething x (safeDec y)
      , isSomething x (safeInc y)
      , isWallOrDoor (safeDec x) y
      , isWallOrDoor (safeInc x) y
      ]
    let [a, b, c, d] = l
    return $ a && b && not (c && d)

stepAndSend :: Maybe Text -> Command -> AppM (Text, Bool)
stepAndSend edit cmd = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask

  (renderedGrid, message, gameOver) <- lift
    $ step cmd renderGrid (pure . unlines . unMessage)
  let text = message <> "\n" <> renderedGrid

  case edit of
    Nothing -> do
      timestamp <-
        liftIO $ sendMessage session token channelID text <&> fromJust
      addReacts timestamp
      return (timestamp, gameOver)
    Just timestamp -> do
      void . liftIO . async $ editMessage session token channelID timestamp text

      return (timestamp, gameOver)

handleMsg
  :: Text -> Chan (Text, Command) -> IORef (Maybe Text) -> EventHandler IO
handleMsg timestamp channel userRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> do
      unless (u == rogueUserId) $ do
        writeChan channel (timestamp, fromReact e)
        liftIO $ writeIORef userRef (Just u)
      return BasicRes

    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes

app :: Chan (Text, Command) -> IORef (Maybe Text) -> AppM ()
app channel userRef = do
  context        <- ask
  (timestamp, _) <- do
    stepAndSend Nothing Noop
  msgThread <- liftIO . async $ wsConnect
    (ctxSession context)
    (ctxWSToken context)
    (handleMsg timestamp channel userRef)
  timerThread <- liftIO . async . forever $ do
    threadDelay 1000000
    writeChan channel (timestamp, IncrementTimer)
  let
    gameLoop = do
      (timestamp', command ) <- liftIO $ readChan channel
      (_         , gameOver) <- stepAndSend (Just timestamp') command
      if gameOver
        then do
          let leaderboardPath = toString $ ctxLeaderboardFile context
          leaderboard <- liftIO $ doesFileExist leaderboardPath >>= \case
            True ->
              readFileBS leaderboardPath
                >>= maybe (die "Failed to read leaderboard file") pure
                .   decodeStrict
            False -> pure $ Leaderboard []
          void $ stepAndSend (Just timestamp') (DisplayLeaderboard leaderboard)

          (depth, secs) <- lift getLeaderboardInfo
          currentTime   <- liftIO getCurrentTime
          userName      <- liftIO $ readIORef userRef >>= maybe
            (pure "a ghost?")
            (getUserName (ctxSession context) (ctxAPIToken context))
          let newLeaderboard = withLeaderboard
                (<> [ LeaderboardEntry { leName  = userName
                                       , leTime  = currentTime
                                       , leDepth = depth
                                       , leSecs  = secs
                                       }
                    ]
                )
                leaderboard
          liftIO $ encodeFile leaderboardPath newLeaderboard

          cancel msgThread
          cancel timerThread
        else gameLoop
  gameLoop

main :: IO ()
main = do
  let envVarNames =
        [ "SLACK_API_TOKEN"
        , "SLACK_WS_TOKEN"
        , "RL_CHANNEL_NAME"
        , "LEADERBOARD_FILE"
        ]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars of
    [Just at, Just wst, Just cn, Just lf] -> do
      session   <- S.newSession
      channelID <- getChannelID session at cn
      let context = Context { ctxSession         = session
                            , ctxAPIToken        = at
                            , ctxWSToken         = wst
                            , ctxChannelID       = channelID
                            , ctxLeaderboardFile = lf
                            }
      channel    <- newChan
      latestUser <- newIORef Nothing
      forever $ runRogue (runReaderT (app channel latestUser) context)

    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
