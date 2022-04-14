{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Game
  ( RogueM
  , TileGrid
  , Leaderboard(..)
  , LeaderboardEntry(..)
  , withLeaderboard
  , Command(..)
  , Message(..)
  , Tile(..)
  , Portal(..)
  , represent
  , populateWorld
  , leaderboardText
  , getLeaderboardInfo
  , step
  , runRogue
  ) where

import           Apecs                   hiding ( System )
import           Apecs.Core              hiding ( System )
import           Apecs.Util
import           Relude                  hiding ( Map
                                                , Set
                                                , get
                                                , modify
                                                )

import           Game.TH
import           Utils

import qualified Control.Lens                  as L
import           Control.Monad.Random    hiding ( fromList )
import           Data.Default
import           Data.Graph.AStar
import           Data.List                      ( (!!)
                                                , union
                                                )
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Data.Vector.Fixed             as FVec
import qualified Data.Vector.Unboxed           as Vec


-- components
-------------

-- globals

defComponent "Message"
  [("unMessage", mkType ''[] [''Text])]
  ''Global

instance Semigroup Message where
  x <> y = Message $ unMessage x <> unMessage y
instance Monoid Message where
  mempty = Message []

withMessage :: ([Text] -> [Text]) -> Message -> Message
withMessage f = Message . f . unMessage

data GameStage = Intro | MainGame | GameOver
defComponent "InGameStage"
  [("_unGameStage", mkType ''GameStage [])]
  ''Global

instance Semigroup InGameStage where
  InGameStage GameOver <> _                    = InGameStage GameOver
  _                    <> InGameStage GameOver = InGameStage GameOver
  InGameStage MainGame <> _                    = InGameStage MainGame
  _                    <> InGameStage MainGame = InGameStage MainGame
  _                    <> _                    = InGameStage Intro
instance Monoid InGameStage where
  mempty = InGameStage Intro

defComponent "TurnsElapsed"
  [("unTurnsElapsed", mkType ''Int [])]
  ''Global

instance Semigroup TurnsElapsed where
  TurnsElapsed x <> TurnsElapsed y = TurnsElapsed (x + y)
instance Monoid TurnsElapsed where
  mempty = TurnsElapsed 0

withTurnsElapsed :: (Int -> Int) -> TurnsElapsed -> TurnsElapsed
withTurnsElapsed f = TurnsElapsed . f . unTurnsElapsed

defComponent "SecsElapsed"
  [("unSecsElapsed", mkType ''Int [])]
  ''Global

instance Semigroup SecsElapsed where
  SecsElapsed x <> SecsElapsed y = SecsElapsed (x + y)
instance Monoid SecsElapsed where
  mempty = SecsElapsed 0

withSecsElapsed :: (Int -> Int) -> SecsElapsed -> SecsElapsed
withSecsElapsed f = SecsElapsed . f . unSecsElapsed

defComponent "Depth"
  [("unDepth", mkType ''Int [])]
  ''Global

instance Semigroup Depth where
  Depth x <> Depth y = Depth (x + y)
instance Monoid Depth where
  mempty = Depth 0

withDepth :: (Int -> Int) -> Depth -> Depth
withDepth f = Depth . f . unDepth

-- abilities

defComponent "CanMove" [] ''Map
defComponent "HasHealth"
  [("unHealth", mkType ''Int [])]
  ''Map
defComponent "HasLocation"
  [ ("posX", mkType ''Int [])
  , ("posY", mkType ''Int [])
  ]
  ''Map

withHealth :: (Int -> Int) -> HasHealth -> HasHealth
withHealth f = HasHealth . f . unHealth

deriving instance Eq HasLocation

-- terrain

defComponent "IsDoor" [] ''Map
defComponent "IsWall" [] ''Map

-- objects

data Portal = Blue | Orange deriving Eq
defComponent "IsFire" [] ''Map
defComponent "IsPortal"
  [("portalType", mkType ''Portal [])]
  ''Map
defComponent "IsPotion"
  [("unPotion", mkType ''Int [])]
  ''Map

-- staircase

defComponent "IsStaircase" [] ''Map

-- alive

defComponent "IsEvil" [] ''Map
defComponent "IsPlayer" [] ''Unique


-- world
--------

makeWorld "World" componentNames

-- TODO rename the other RogueM, whose name is less fitting
type RandIOM = RandT StdGen IO
type RogueM = SystemT World RandIOM


-- utilities
------------

-- component polymorphism

type Gettable a = (Get World RandIOM a, Get World RogueM a)
type Settable a = Set World RandIOM a
type Memberable a = (Members World RandIOM a, Members World RogueM a)
data ComponentBox = forall a . (Gettable a, Settable a, Memberable a) => C a
data ProxyBox = forall a . (Gettable a, Settable a, Memberable a) => P ( Proxy
                                                                           a
                                                                       )

existsBoxed :: Entity -> ProxyBox -> RogueM Bool
existsBoxed e (P (_ :: Proxy c)) = exists e (Proxy :: Proxy c)


setBoxed :: Entity -> ComponentBox -> RogueM ()
setBoxed e (C c) = set e c

mkEntity :: [ComponentBox] -> RogueM Entity
mkEntity = (nextEntity >>=) . mkEntity'
 where
  mkEntity' (s : ss) e = setBoxed e s >> mkEntity' ss e
  mkEntity' []       e = pure e

mkEntity_ :: [ComponentBox] -> RogueM ()
mkEntity_ = void . mkEntity


-- members

members :: Memberable c => Proxy c -> RogueM [Entity]
members (_ :: Proxy c) = do
  storage :: Storage c <- getStore
  memberVec            <- lift $ explMembers storage
  (pure . map Entity . Vec.toList) memberVec


-- deletion

mkDelete localComponentNames


-- map

type EntityGrid = Matrix (Maybe Entity)
type TileGrid = Matrix (Maybe Tile)

emptyMapE :: EntityGrid
emptyMapE = (Matrix . FVec.replicate . FVec.replicate) Nothing

emptyMapT :: TileGrid
emptyMapT = (Matrix . FVec.replicate . FVec.replicate) Nothing

maxCoord :: Int
maxCoord = matrixSize - 1


-- message

appendMessage :: Text -> RogueM ()
appendMessage text = modify global $ withMessage (text :)


-- location

fromLocation :: HasLocation -> (Int, Int)
fromLocation (HasLocation x y) = (x, y)

entityExistsAt :: Int -> Int -> RogueM Bool
entityExistsAt x y =
  cfold (\found comp -> found || fromLocation comp == (x, y)) False

findEntityAt :: Gettable c => Int -> Int -> Proxy c -> RogueM (Maybe Entity)
findEntityAt x y (_ :: Proxy c) =
  members (Proxy :: Proxy (HasLocation, c))
    >>= mapM get
    <&> listToMaybe
    .   mapMaybe
          (\(entity, HasLocation x' y') ->
            if x == x' && y == y' then Just entity else Nothing
          )

entityWithExistsAt :: Gettable c => Int -> Int -> Proxy c -> RogueM Bool
entityWithExistsAt x y p = findEntityAt x y p <&> isJust

wallExistsAt :: Int -> Int -> RogueM Bool
wallExistsAt x y =
  cfold (\found (comp, IsWall) -> found || fromLocation comp == (x, y)) False

randomCoord :: RogueM (Int, Int)
randomCoord = do
  x <- lift $ getRandomR (0, maxCoord)
  y <- lift $ getRandomR (0, maxCoord)
  pure (x, y)

randomCoordNoEdge :: RogueM (Int, Int)
randomCoordNoEdge = do
  x <- lift $ getRandomR (1, maxCoord - 1)
  y <- lift $ getRandomR (1, maxCoord - 1)
  pure (x, y)

mkEntityOnEmpty :: [ComponentBox] -> RogueM Entity
mkEntityOnEmpty ss = do
  (x, y) <- randomCoord
  entityExistsAt x y >>= \occupied -> if occupied
    then mkEntityOnEmpty ss
    else mkEntity $ C (HasLocation x y) : ss

mkEntityOnEmpty_ :: [ComponentBox] -> RogueM ()
mkEntityOnEmpty_ ss = do
  (x, y) <- randomCoord
  entityExistsAt x y >>= \occupied -> if occupied
    then mkEntityOnEmpty_ ss
    else mkEntity_ $ C (HasLocation x y) : ss


-- difficulty
-------------

getDifficulty :: RogueM Double
getDifficulty = get global <&> \(SecsElapsed secs, Depth depth) ->
  (fromIntegral secs * (fromIntegral depth + 2)) ** 0.7 + 100


-- comp gen
-----------

randomItemComponents :: RogueM [ComponentBox]
randomItemComponents = do
  regenAmount <- lift $ getRandomR (3, 7)
  difficulty  <- getDifficulty
  randomIO <&> \n ->
    if n > (difficulty / 300) then [C $ IsPotion regenAmount] else [C IsFire]


-- commands
-----------

newtype Leaderboard = Leaderboard
  { unLeaderboard :: [LeaderboardEntry]
  } deriving Eq

data LeaderboardEntry = LeaderboardEntry
  { leName  :: Text
  , leTime  :: UTCTime
  , leDepth :: Int
  , leSecs  :: Int
  }
  deriving Eq

withLeaderboard
  :: ([LeaderboardEntry] -> [LeaderboardEntry]) -> Leaderboard -> Leaderboard
withLeaderboard f (Leaderboard entries) = Leaderboard (f entries)

-- `IncrementTimer` is not really a command, 
-- but indicates that most systems shouldn't run
data Command = IncrementTimer
             | DisplayLeaderboard Leaderboard
             | Noop
             | Move (Int, Int)
             | Drink
             | Die
             deriving Eq


-- initialization
-----------------

mkWalls :: RogueM ()
mkWalls = mkWalls' 25 where
  mkWalls' :: Int -> RogueM ()
  mkWalls' 0 = return ()
  mkWalls' n = do
    (x, y) <- randomCoordNoEdge

    let
      pos1 = if even n then posX else posY
      dim1 = if even n then x else y
      pos2 = if even n then posY else posX
      dim2 = if even n then y else x

      adjEntityLocs :: RogueM [HasLocation]
      adjEntityLocs = cfold
        (\ls loc -> if abs (pos2 loc - dim2) < 3 && pos1 loc - dim1 == 0
          then loc : ls
          else ls
        )
        []

      constrainWalls isMin =
        (\case
            [] -> pure $ if isMin then Just 0 else Just (matrixSize - 1)
            (e, loc) : _ ->
              existsBoxed e (P (Proxy :: Proxy IsDoor))
                ||^ existsBoxed e (P (Proxy :: Proxy IsPlayer))
                >>= \blocked -> pure $ if blocked
                      then Nothing
                      else (Just . (if isMin then safeInc else safeDec) . pos1)
                        loc
          )
          .   (if isMin then reverse else id)
          .   sortOn (pos1 . snd)
          .   filter ((if isMin then (>=) else (<=)) dim1 . pos1 . snd)
          .   filter ((== dim2) . pos2 . snd)
          <=< mapM get

      safeInc n' = if n' == matrixSize - 1 then n' else succ n'
      safeDec n' = if n' == 0 then n' else pred n'

    entities  <- members (Proxy :: Proxy HasLocation)
    dimRanges <- sequence
      [ constrainWalls isMin entities | isMin <- [True, False] ]

    valid <- null <$> adjEntityLocs
    if valid
      then case dimRanges of
        [Just min', Just max'] -> do
          newWalls <- sequence $ if even n
            then
              [ mkEntity [C (HasLocation x' y), C IsWall]
              | x' <- [min' .. max']
              ]
            else
              [ mkEntity [C (HasLocation x y'), C IsWall]
              | y' <- [min' .. max']
              ]
          doorPos <- lift $ getRandomR (0, length newWalls - 1)
          set (newWalls !! doorPos) (Not :: Not IsWall, IsDoor)
          mkWalls' (n + 1)
        _ -> mkWalls' (n - 1)
      else mkWalls' (n - 1)

populateWorld :: Maybe Entity -> RogueM ()
populateWorld existingPlayer = do
  (staircase, player) <- populateWorld'
  HasLocation sx sy   <- get staircase
  HasLocation px py   <- get player

  minPathLength       <- getDifficulty <&> round . (/ 20)
  maxPathLength       <- getDifficulty <&> round . (/ 10)

  pathfind (px, py) (sx, sy) >>= \case
    Nothing   -> tryAgain
    Just path -> unless
      (length path > minPathLength && length path < maxPathLength)
      tryAgain

  -- i'm evil and horrible
  entities <- members (Proxy :: Proxy HasLocation)
  forM_ entities
    $ (\entity ->
        represent entity >>= \repr -> when (repr == ErrorTile) (delete entity)
      )
 where
  populateWorld' = do
    difficulty <- getDifficulty
    enemyCount <- (+ floor (difficulty / 120)) <$> lift (getRandomR (1, 4))
    itemCount  <- (+ floor (difficulty / 100)) <$> lift (getRandomR (1, 4))

    mkWalls
    replicateM_ enemyCount
      $ mkEntityOnEmpty_ [C CanMove, C (HasHealth 3), C IsEvil]
    replicateM_ itemCount $ randomItemComponents >>= mkEntityOnEmpty_
    forM_ [C (IsPortal Blue), C (IsPortal Orange)] $ mkEntityOnEmpty_ . (: [])
    staircase <- mkEntityOnEmpty [C IsStaircase]
    player    <- case existingPlayer of
      Just player -> pure player
      Nothing     -> mkEntityOnEmpty [C CanMove, C (HasHealth 10), C IsPlayer]

    pure (staircase, player)

  tryAgain = do
    cmapM_ $ \(HasLocation _ _, Not :: Not IsPlayer, entity) -> delete entity
    populateWorld existingPlayer


-- movement
-----------

getNeighbors :: (Int, Int) -> RogueM (HashSet (Int, Int))
getNeighbors (x, y) =
  fmap fromList
    . filterM (fmap not . uncurry wallExistsAt)
    . filter
        (\(x', y') -> x' >= 0 && x' < matrixSize && y' >= 0 && y' < matrixSize)
    $ [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

portalReplace :: (Int, Int) -> RogueM (Int, Int)
portalReplace (x, y) = do
  portalHere <- cfold
    (\acc (HasLocation x' y', IsPortal p) ->
      if (x, y) == (x', y') then Just p else acc
    )
    Nothing
  case portalHere of
    Just Blue ->
      cfold
          (\acc (HasLocation x' y', IsPortal p) ->
            if p == Orange then Just (x', y') else acc
          )
          Nothing
        <&> fromMaybe (x, y)
    Just Orange ->
      cfold
          (\acc (HasLocation x' y', IsPortal p) ->
            if p == Blue then Just (x', y') else acc
          )
          Nothing
        <&> fromMaybe (x, y)
    Nothing -> pure (x, y)

getNeighborsPortal :: (Int, Int) -> RogueM (HashSet (Int, Int))
getNeighborsPortal =
  getNeighbors >=> mapM portalReplace . toList >=> pure . fromList

-- TODO account for portals in pathfinding
pathfind :: (Int, Int) -> (Int, Int) -> RogueM (Maybe [(Int, Int)])
pathfind begin dest = aStarM getNeighbors
                             (\_ _ -> pure (1 :: Int))
                             (const $ pure 1)
                             (pure . (== dest))
                             (pure begin)

-- also handles combat
moveTo :: Entity -> Int -> Int -> RogueM ()
moveTo entity destX destY = do
  HasLocation curX curY <- get entity
  getNeighbors (curX, curY)
    >>= flip when attackOrMove
    .   ((destX, destY) `elem`)
    .   toList
 where
  attackOrMove = do
    (destX', destY') <- portalReplace (destX, destY)
    findEntityAt destX destY (Proxy :: Proxy HasHealth) >>= maybe
      (set entity $ HasLocation destX' destY')
      (flip modify $ withHealth pred)


-- tiles
--------

data Tile = WallTile
          | DoorTile
          | EvilTile
          | FireTile
          | PotionTile
          | PortalTile Portal
          | StaircaseTile
          | PlayerTile
          | ErrorTile
          deriving Eq


-- systems
----------

data System = System
  { qualifier      :: [ProxyBox]
  , action         :: Command -> Entity -> RogueM ()
  , transRepr      :: Entity -> Tile -> RogueM Tile
  , transName      :: Entity -> Text -> RogueM Text
  , forbidIncTimer :: Bool
  }

instance Default System where
  def = System { qualifier      = []
               , action         = \_ _ -> pure ()
               , transRepr      = const pure
               , transName      = const pure
               , forbidIncTimer = True
               }

qualified :: Entity -> [ProxyBox] -> RogueM Bool
qualified = allM . existsBoxed


trivialRender :: ProxyBox -> Tile -> Text -> System
trivialRender comp tile name' = def { qualifier = [comp]
                                    , transRepr = \_ _ -> pure tile
                                    , transName = \_ _ -> pure name'
                                    }

moveEvil :: System
moveEvil = def
  { qualifier = [$(pb "HasLocation"), $(pb "CanMove"), $(pb "IsEvil")]
  , action    = \_ e -> do
                  HasLocation evilX evilY <- get e
                  playerLoc <- cfold (\_ (loc, IsPlayer) -> Just loc) Nothing
                  case playerLoc of
                    Just (HasLocation playerX playerY) -> do
                      path <- pathfind (evilX, evilY) (playerX, playerY)
                      maybe (pure ()) (uncurry $ moveTo e)
                        . (listToMaybe =<<)
                        $ path
                    Nothing -> pure ()
  }

-- TODO perhaps generalize?
drinkPotion :: System
drinkPotion = def
  { qualifier = [$(pb "IsPlayer")]
  , action    = \c e -> case c of
                  Drink -> get e >>= \(HasLocation x y) ->
                    findEntityAt x y (Proxy :: Proxy IsPotion) >>= maybe
                      (pure ())
                      (\e' -> do
                        get e' >>= modify e . withHealth . (+) . unPotion
                        delete e'
                        appendMessage "you feel rejuvenated..."
                      )
                  _ -> pure ()
  }

spreadFire :: System
spreadFire = def
  { qualifier = [$(pb "IsFire")]
  , action    = \_ e -> do
    neighbors <- get e >>= getNeighborsPortal . fromLocation
    forM_ neighbors $ \(x, y) -> do
      spreadTo <- (lift getRandom :: RogueM Double) <&> (< 0.05)
      fireAt   <- cfold
        (\found (HasLocation x' y', IsFire) -> found || (x, y) == (x', y'))
        False
      when (spreadTo && not fireAt) $ mkEntity_ [C (HasLocation x y), C IsFire]
  }

takeFireDamage :: System
takeFireDamage = def
  { qualifier = [$(pb "HasHealth")]
  , action    = \_ e -> do
                  HasLocation x y <- get e
                  onFire          <- cfold
                    (\found (HasLocation fireX fireY, IsFire) ->
                      found || (fireX, fireY) == (x, y)
                    )
                    False
                  when onFire $ do
                    modify e (withHealth pred)
                    name e >>= appendMessage . (<> " burns...")
  }

descendStaircase :: System
descendStaircase = def
  { qualifier = [$(pb "HasLocation"), $(pb "IsPlayer")]
  , action    = \_ e -> get e >>= \(HasLocation x y) ->
    whenM (entityWithExistsAt x y (Proxy :: Proxy IsStaircase)) $ do
      modify global $ withDepth succ
      cmapM_ $ \(HasLocation _ _, entity, Not :: Not IsPlayer) -> delete entity
      populateWorld (Just e)
  }

systems :: [System]
systems =
  [ -- rendering and naming
    trivialRender $(pb "IsWall")   WallTile   "a wall"
  , trivialRender $(pb "IsDoor")   DoorTile   "a door"
  , trivialRender $(pb "IsEvil")   EvilTile   "a rat"
  , trivialRender $(pb "IsFire")   FireTile   "fire"
  , trivialRender $(pb "IsPotion") PotionTile "a potion"
  , def { qualifier = [$(pb "IsPortal")]
        , transRepr = \e _ -> get e <&> PortalTile . portalType
        , transName = \_ _ -> pure "a portal"
        }
  , trivialRender $(pb "IsStaircase") StaircaseTile "a staircase"
  , trivialRender $(pb "IsPlayer")    PlayerTile    "the player"
    -- active
  , def
    { qualifier = [$(pb "HasHealth"), $(pb "IsPlayer")]
    , action    = \c e -> case c of
                    Die -> set e $ HasHealth 0
                    _   -> pure ()
    }
  , def
    { qualifier = [$(pb "HasLocation"), $(pb "CanMove"), $(pb "IsPlayer")]
    , action    = \c e -> case c of
                    Move (x, y) -> do
                      HasLocation x' y' <- get e
                      moveTo e (x + x') (y + y')
                    _ -> pure ()
    }
  , moveEvil
  , drinkPotion
    -- passive
  , spreadFire
  , takeFireDamage
  , def
    { qualifier = [$(pb "HasHealth")]
    , action    = \_ e -> get e >>= \(HasHealth x) -> when (x <= 0) $ do
      name e >>= appendMessage . (<> " has died!")
      whenM (exists e (Proxy :: Proxy IsPlayer)) $ do
        get global >>= \(Depth n) ->
          appendMessage ("you survived to depth " <> show n <> ".")
        set global $ InGameStage GameOver
      delete e
    }
    -- message
  , def
    { qualifier = [$(pb "HasHealth"), $(pb "IsPlayer")]
    , action    = \_ e -> get e >>= \(HasHealth hp, Depth d) -> appendMessage
                    ("you have " <> show hp <> " hp (depth " <> show d <> ")")
    }
  -- descension
  , descendStaircase
  ]


-- global systems
-----------------

clearMessage :: Command -> RogueM ()
clearMessage command = case command of
  IncrementTimer -> pure ()
  _              -> set global $ Message ["..."]

displayIntro :: Command -> RogueM ()
displayIntro _ = get global >>= \case
  (InGameStage Intro, TurnsElapsed 2) -> do
    appendMessage "you enter the dungeon..."
    populateWorld Nothing
    set global $ InGameStage MainGame
  (InGameStage Intro, _) -> set global $ Message [introMessage]
  _                      -> pure ()
 where
  introMessage
    = "welcome to dungeon of the slack\n\n\
      \react with :tw_arrow_up::tw_arrow_right::tw_arrow_down::tw_arrow_left: to move\n\
      \react with :tw_hourglass: to wait\n\
      \react with :tw_tea: to drink a potion\n\
      \react with :tw_skull: to die instantly\n\n\
      \move quickly; you will find that the dungeon becomes less forgiving as time progresses"

incrementTurns :: Command -> RogueM ()
incrementTurns command =
  unless (command == IncrementTimer) $ modify global $ withTurnsElapsed succ

globalSystemsPre :: [Command -> RogueM ()]
globalSystemsPre = [clearMessage, displayIntro, incrementTurns]


displayLeaderboard :: Command -> RogueM ()
displayLeaderboard = \case
  DisplayLeaderboard leaderboard -> do
    displayed <- liftIO (leaderboardText leaderboard)
    appendMessage displayed
  _ -> pure ()

incrementSecs :: Command -> RogueM ()
incrementSecs = \case
  IncrementTimer -> get global >>= \case
    InGameStage MainGame -> modify global $ withSecsElapsed succ
    _                    -> pure ()
  _ -> pure ()

displaySecs :: Command -> RogueM ()
displaySecs _ = do
  (InGameStage stage, SecsElapsed secs) <- get global
  case stage of
    MainGame -> modify global . withMessage $ L.set
      (L.ix 0)
      ((<> " seconds...") . show $ secs)
    _ -> pure ()

reverseMessage :: Command -> RogueM ()
reverseMessage command = case command of
  IncrementTimer -> pure ()
  _              -> modify global $ withMessage reverse

globalSystemsPost :: [Command -> RogueM ()]
globalSystemsPost =
  [displayLeaderboard, reverseMessage, incrementSecs, displaySecs]


-- run
-------

build :: (System -> Entity -> a -> RogueM a) -> a -> Entity -> RogueM a
build f d entity = foldl'
  (>=>)
  (const $ pure d)
  (map (\s -> whenQualified s . (entity &) . f $ s) systems)
  entity
 where
  whenQualified system m tile = qualified entity (qualifier system)
    >>= \qual -> (if qual then m else pure) tile

represent :: Entity -> RogueM Tile
represent = build transRepr ErrorTile

name :: Entity -> RogueM Text
name = build transName "something"


-- lmfao
compareEntities :: Entity -> Entity -> RogueM Ordering
compareEntities e1 e2 = do
  e1player    <- exists e1 (Proxy :: Proxy IsPlayer)
  e2player    <- exists e2 (Proxy :: Proxy IsPlayer)
  e1evil      <- exists e1 (Proxy :: Proxy IsEvil)
  e2evil      <- exists e2 (Proxy :: Proxy IsEvil)
  e1staircase <- exists e1 (Proxy :: Proxy IsStaircase)
  e2staircase <- exists e2 (Proxy :: Proxy IsStaircase)

  case (e1player, e2player, e1evil, e2evil, e1staircase, e2staircase) of
    (True, False, _, _, _, _) -> pure GT
    (False, True, _, _, _, _) -> pure LT
    (_, _, True, False, _, _) -> pure GT
    (_, _, False, True, _, _) -> pure LT
    (_, _, _, _, True, False) -> pure GT
    (_, _, _, _, False, True) -> pure LT
    _ -> pure EQ

getGrid :: RogueM TileGrid
getGrid = get global >>= \case
  InGameStage Intro -> pure $ compose
    [ mset x y (Just ErrorTile)
    | x <- [0 .. matrixSize]
    , y <- [0 .. matrixSize]
    ]
    emptyMapT
  InGameStage _ ->
    members (Proxy :: Proxy HasLocation)
      >>= mapM get
      >>= foldlM go emptyMapE
      >>= mapM (maybe (pure Nothing) (represent >=> pure . Just))
 where
  go grid (entity, entityLoc) = do
    comparison <- case mget (posX entityLoc) (posY entityLoc) grid of
      Just existingEntity -> compareEntities entity existingEntity
      Nothing             -> pure GT
    case comparison of
      LT -> pure grid
      _  -> pure $ mset (posX entityLoc) (posY entityLoc) (Just entity) grid

executeStep :: Command -> RogueM ()
executeStep command = do
  forM_ globalSystemsPre (\f -> f command)
  SecsElapsed secs <- get global
  let realTimeStep = secs >= 180 && even secs
  forM_
    systems
    (\s ->
      unless (forbidIncTimer s && command == IncrementTimer && not realTimeStep)
        $ do
            members' <- mapM (\(P (_ :: p)) -> members (Proxy :: p))
                             (qualifier s)
            mapM_
              (\e -> whenM (qualified e (qualifier s)) $ action s command e)
              (foldl' union [] members')
    )
  forM_ globalSystemsPost (\f -> f command)

leaderboardText :: Leaderboard -> IO Text
leaderboardText (Leaderboard entries) = do
  currentTime <- liftIO getCurrentTime
  let oneWeek = secondsToNominalDiffTime 604800
  pure
    $  "leaderboard:\n"
    <> ( unlines
       . map displayEntry
       . take 10
       . sortOn (\e -> Down (leDepth e, leSecs e))
       . filter ((< oneWeek) . diffUTCTime currentTime . leTime)
       )
         entries
 where
  displayEntry LeaderboardEntry {..} =
    leName <> ": depth " <> show leDepth <> ", " <> show leSecs <> " secs"

getLeaderboardInfo :: RogueM (Int, Int)
getLeaderboardInfo = do
  (Depth depth, SecsElapsed secs) <- get global
  pure (depth, secs)

step
  :: Command
  -> (TileGrid -> RogueM a)
  -> (Message -> RogueM b)
  -> RogueM (a, b, Bool)
step command renderGrid renderMessage = do
  executeStep command
  grid     <- getGrid >>= renderGrid
  message  <- get global >>= renderMessage
  gameOver <- get global >>= \case
    InGameStage GameOver -> pure True
    _                    -> pure False
  pure (grid, message, gameOver)

runRogue :: RogueM a -> IO a
runRogue f = do
  rng   <- newStdGen
  world <- initWorld
  runRandT (runSystem f world) rng <&> fst
