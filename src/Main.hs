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

import           Control.Concurrent             ( forkIO )
import           Control.Monad.Random
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )
import           Network.Wreq.Session           ( Session )
import qualified Network.Wreq.Session          as S
import           System.Environment             ( lookupEnv )


data Context = Context
  { ctxSession   :: Session
  , ctxAPIToken  :: Text
  , ctxWSToken   :: Text
  , ctxChannelID :: Text
  }

type RogueM = ReaderT Context IO


-- user id hardcoded for convenience. sorry!
rogueUserId :: Text
rogueUserId = "U02MTTW1XND"

addReacts :: Text -> RogueM ()
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
  "tw_arrow_up"    -> North
  "tw_arrow_right" -> East
  "tw_arrow_down"  -> South
  "tw_arrow_left"  -> West
  "tw_tea"         -> Drink
  _                -> Noop

renderGrid :: EntityGrid -> Text
renderGrid es =
  fromString . concat . intercalate ["\n"] . m2l . fmap fromCoord $ coordMatrix
 where
  coordMatrix = (l2m . chunksOf matrixSize)
    [ (x, y) | y <- [0 .. matrixSize - 1], x <- [0 .. matrixSize - 1] ]

  fromCoord (x, y) =
    (fromEntityRepr (vertical (x, y)) . fmap represent) (mget x y es)

  fromEntityRepr vertical' = \case
    Just DefaultTile -> ":rogue__default:"
    Just PlayerTile  -> ":rogue__player:"
    Just GoalTile    -> ":rogue__goal:"
    Just WallTile ->
      if vertical' then ":rogue__wall_vert:" else ":rogue__wall_horiz:"
    Just DoorTile ->
      if vertical' then ":rogue__door_vert:" else ":rogue__door_horiz:"
    Just EvilTile      -> ":rogue__rat:"
    Just PotionTile    -> ":rogue__potion:"
    Just InPortalTile  -> ":rogue__portal_in:"
    Just OutPortalTile -> ":rogue__portal_out:"
    Nothing            -> ":rogue__blank:"

  isWallOrDoor x' y' = maybe False (\e -> isWall e || isDoor e) $ mget x' y' es
  safeInc a = if a < matrixSize - 1 then a + 1 else a
  safeDec a = if a > 0 then a - 1 else a
  vertical (x, y) =
    isWallOrDoor x (safeDec y) && isWallOrDoor x (safeInc y) && not
      (isWallOrDoor (safeDec x) y && isWallOrDoor (safeInc x) y)

stepAndSend :: Maybe Text -> Command -> GameState -> RogueM (Text, GameState)
stepAndSend edit cmd gameState = do
  Context { ctxSession = session, ctxAPIToken = token, ctxChannelID = channelID } <-
    ask
  let (renderedGrid, newState) =
        runState (step renderGrid) $ setCommand cmd gameState
      text = (unlines . getMessage $ newState) <> "\n" <> renderedGrid
  case edit of
    Nothing -> do
      timestamp <-
        liftIO $ sendMessage session token channelID text <&> fromJust
      addReacts timestamp
      return (timestamp, newState)
    Just timestamp -> do
      void . liftIO . forkIO $ editMessage session
                                           token
                                           channelID
                                           timestamp
                                           text
      return (timestamp, newState)

handleMsg :: Text -> IORef GameState -> EventHandler RogueM
handleMsg timestamp gsRef msg = do
  putStrLn $ "Message from socket: " <> show msg
  case msg of
    ReactionAdd e u -> do
      unless (u == rogueUserId) $ do
        let cmd = fromReact e
        (_, newState) <- readIORef gsRef >>= stepAndSend (Just timestamp) cmd
        writeIORef gsRef newState
      return BasicRes

    _ -> do
      putStrLn $ "Can't handle event: " <> show msg
      return NoRes

app :: RogueM ()
app = do
  context                <- ask
  (timestamp, gameState) <-
    liftIO (evalRandIO $ mkGameState Noop) >>= stepAndSend Nothing Noop
  newIORef gameState
    >>= liftIO
    .   wsConnect (ctxSession context) (ctxWSToken context)
    .   (\g e -> runReaderT (handleMsg timestamp g e) context)
    -- i think the message handler has to be IO
    -- runSecureClient is monomorphic in its argument
    -- so...whatever

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
      runReaderT app context

    _ ->
      void
        .  die
        $  "Can't find some of the following environment variables: "
        <> intercalate ", " envVarNames
