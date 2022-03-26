{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module Main
  ( main
  ) where

import           Relude hiding ( get )

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
import           Web.Scotty


data Context = Context
  { ctxSession         :: Session
  , ctxAPIToken        :: Text
  , ctxWSToken         :: Text
  , ctxChannelID       :: Text
  , ctxLeaderboardFile :: Text
  }

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

-- renderGrid is a mess! i'm quite aware
renderGrid :: TileGrid -> RogueM Text
renderGrid es =
  mapM fromCoord coordMatrix <&> fromString . concat . intercalate ["\n"] . m2l
 where
  coordMatrix = (l2m . chunksOf matrixSize)
    [ (x, y) | y <- [0 .. matrixSize - 1], x <- [0 .. matrixSize - 1] ]

  fromCoord (x, y) = fromEntityRepr (vertical (x, y)) (mget x y es)

  fromEntityRepr vertical' = pure . \case
    Just WallTile ->
      if vertical' then ":rogue__wall_vert:" else ":rogue__wall_horiz:"
    Just DoorTile ->
      if vertical' then ":rogue__door_vert:" else ":rogue__door_horiz:"
    Just EvilTile            -> ":rogue__rat:"
    Just FireTile            -> ":rogue__fire:"
    Just PotionTile          -> ":rogue__potion:"
    Just (PortalTile Blue  ) -> ":rogue__portal_out:"
    Just (PortalTile Orange) -> ":rogue__portal_in:"
    Just StaircaseTile       -> ":rogue__staircase:"
    Just PlayerTile          -> ":rogue__player:"
    Just ErrorTile           -> ":rogue__default:"
    Nothing                  -> ":rogue__blank:"

  isWallOrDoor' = (== WallTile) ||$ (== DoorTile)
  isWallOrDoor x' y' = maybe False isWallOrDoor' $ mget x' y' es
  isSomething x' y' = isJust $ mget x' y' es
  safeInc a = if a < matrixSize - 1 then a + 1 else a
  safeDec a = if a > 0 then a - 1 else a
  vertical (x, y) = a && b && not (c && d)   where
    [a, b, c, d] =
      [ isSomething x (safeDec y)
      , isSomething x (safeInc y)
      , isWallOrDoor (safeDec x) y
      , isWallOrDoor (safeInc x) y
      ]

stepAndSend :: Maybe Text -> Text -> Command -> GameM (Text, Bool)
stepAndSend edit user cmd = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask

  (renderedGrid, message, gameOver) <- lift $ step
    cmd
    renderGrid
    (pure . (("(<@" <> user <> ">'s game)\n\n") <>) . unlines . unMessage)
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
      return $ SlashCommandRes "starting a new game..." False

    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes


getLeaderboard :: Text -> IO Leaderboard
getLeaderboard (toString -> path) = doesFileExist path >>= \case
  True ->
    readFileBS path
      >>= maybe (die "Failed to read leaderboard file...") pure
      .   decodeStrict
  False -> pure (Leaderboard [])


initializeGame :: Text -> GameM Text
initializeGame user = stepAndSend Nothing user Noop <&> fst

runGame :: Chan Command -> Text -> Text -> GameM ()
runGame channel timestamp user = do
  void . liftIO . async . forever $ do
    threadDelay 1000000
    writeChan channel IncrementTimer

  let gameLoop = do
        cmd           <- liftIO $ readChan channel
        (_, gameOver) <- stepAndSend (Just timestamp) user cmd
        if gameOver then endGame timestamp user else gameLoop
  gameLoop

endGame :: Text -> Text -> GameM ()
endGame timestamp user = do
  context <- ask

  let leaderboardPath = ctxLeaderboardFile context
  leaderboard <- liftIO (getLeaderboard leaderboardPath)

  void $ stepAndSend (Just timestamp) user (DisplayLeaderboard leaderboard)

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
  liftIO $ encodeFile (toString leaderboardPath) newLeaderboard


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
            gameChannel <- liftIO newChan
            timestamp   <- initializeGame user
            liftIO $ modifyIORef gameChannels (Map.insert timestamp gameChannel)
            runGame gameChannel timestamp user

        void . liftIO . async . runRogue . runReaderT createGame $ context

      NewMemberEvent user -> void . liftIO $ do
        leaderboardText' <-
          getLeaderboard (ctxLeaderboardFile context)
          >>= liftIO
          .   leaderboardText
        sendMessage
          (ctxSession context)
          (ctxAPIToken context)
          (ctxChannelID context)
          (  "hello <@"
          <> user
          <> ">\n\n\
             \i am a small game about dungeon descension; i bear similarities to arcade and roguelike games\n\n\
             \type the command `/rlnewgame` and a new game will await you\n\
             \alternatively, you may explore my recesses: https://github.com/hackclub/rogue\n\n"
          <> leaderboardText'
          )

  cancel wsThread

main :: IO ()
main = do
  let envVarNames =
        [ "SLACK_API_TOKEN"
        , "SLACK_WS_TOKEN"
        , "RL_CHANNEL_NAME"
        , "LEADERBOARD_FILE"
        , "PORT"
        ]
  envVars <- mapM lookupEnv envVarNames

  case map (fmap fromString) envVars :: [Maybe Text] of
    [Just at, Just wst, Just cn, Just lf, Just ((readMaybe . toString) -> Just port)] -> do
      void . async . scotty port . get "/" . html $ "it's up..."

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
