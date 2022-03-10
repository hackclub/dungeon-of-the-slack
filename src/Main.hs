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
import qualified Data.Map                      as Map
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

-- type AppM = ReaderT Context RogueM
type AppM = ReaderT Context IO
type GameM = ReaderT Context RogueM


deriving instance Generic LeaderboardEntry
deriving instance Generic Leaderboard

instance FromJSON LeaderboardEntry
instance ToJSON LeaderboardEntry

instance FromJSON Leaderboard
instance ToJSON Leaderboard


-- user id hardcoded for convenience. sorry!
rogueUserId :: Text
rogueUserId = "U02MTTW1XND"

addReacts :: Text -> GameM ()
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

stepAndSend :: Maybe Text -> Command -> GameM (Text, Bool)
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


data Event = CommandEvent Text Command
           | NewGameEvent Text
           | NewMemberEvent Text

handleMsg :: Chan Event -> Text -> EventHandler IO
handleMsg channel channelID msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    MemberJoin c u -> do
      when (c == channelID) (writeChan channel (NewMemberEvent u))
      return BasicRes

    ReactionAdd e m u -> do
      unless (u == rogueUserId)
             (writeChan channel $ CommandEvent m (fromReact e))
      return BasicRes

    SlashCommand _ u -> do
      writeChan channel $ NewGameEvent u
      return $ SlashCommandRes "Starting a new game..." False

    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes


initializeGame :: GameM Text
initializeGame = stepAndSend Nothing Noop <&> fst

runGame :: Chan Command -> Text -> Text -> GameM ()
runGame channel timestamp user = do
  void . liftIO . async . forever $ do
    threadDelay 1000000
    writeChan channel IncrementTimer

  let gameLoop = do
        cmd           <- liftIO $ readChan channel
        (_, gameOver) <- stepAndSend (Just timestamp) cmd
        if gameOver then endGame timestamp user else gameLoop
  gameLoop

endGame :: Text -> Text -> GameM ()
endGame timestamp user = do
  context <- ask

  let leaderboardPath = toString $ ctxLeaderboardFile context
  leaderboard <- liftIO $ doesFileExist leaderboardPath >>= \case
    True ->
      readFileBS leaderboardPath
        >>= maybe (die "Failed to read leaderboard file") pure
        .   decodeStrict
    False -> pure $ Leaderboard []

  void $ stepAndSend (Just timestamp) (DisplayLeaderboard leaderboard)

  (depth, secs) <- lift getLeaderboardInfo
  currentTime   <- liftIO getCurrentTime
  userName      <- liftIO
    $ getUserName (ctxSession context) (ctxAPIToken context) user
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


app :: AppM ()
app = do
  channel      <- liftIO newChan
  gameChannels <- liftIO $ newIORef Map.empty

  context      <- ask
  wsThread     <- liftIO . async $ wsConnect
    (ctxSession context)
    (ctxWSToken context)
    (handleMsg channel (ctxChannelID context))

  void . forever $ do
    event <- liftIO $ readChan channel
    case event of
      CommandEvent timestamp cmd -> do
        gameChannelMay <- liftIO (readIORef gameChannels)
          <&> Map.lookup timestamp
        case gameChannelMay of
          Just gameChannel -> liftIO . writeChan gameChannel $ cmd
          Nothing          -> putStrLn "Message does not correspond to any game"

      NewGameEvent user -> do
        let
          createGame = do
            gameChannel <- liftIO $ newChan
            timestamp   <- initializeGame
            liftIO $ modifyIORef gameChannels (Map.insert timestamp gameChannel)
            runGame gameChannel timestamp user

        void . liftIO . async . runRogue . runReaderT createGame $ context

      NewMemberEvent user -> do
        void . liftIO $ sendMessage (ctxSession context)
                                    (ctxAPIToken context)
                                    (ctxChannelID context)
                                    ("welcome <@" <> user <> ">...")


  cancel wsThread

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
      runReaderT app context
    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
