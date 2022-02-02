{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Game
  ( RogueM
  , EntityGrid
  , Command(..)
  , Message(..)
  , Tile(..)
  , represent
  , populateWorld
  , step
  , runRogue
  , Portal(..)
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

import           Control.Monad.Random    hiding ( fromList )
import           Data.Default
import           Data.Graph.AStar
import           Data.List                      ( (!!)
                                                , union
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

data GameStage = Intro | MainGame
defComponent "InGameStage"
  [("unGameStage", mkType ''GameStage [])]
  ''Global

instance Semigroup InGameStage where
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

data Portal = In | Out deriving Eq
defComponent "IsFire" [] ''Map
defComponent "IsPortal"
  [("portalType", mkType ''Portal [])]
  ''Map
defComponent "IsPotion"
  [("unPotion", mkType ''Int [])]
  ''Map

-- goal

defComponent "IsGoal" [] ''Map

-- alive

defComponent "IsEvil" [] ''Map
defComponent "IsPlayer" [] ''Unique


-- world
--------

makeWorld "World" [ ''Message
                  , ''InGameStage
                  , ''TurnsElapsed
                  , ''CanMove
                  , ''HasHealth
                  , ''HasLocation
                  , ''IsDoor
                  , ''IsWall
                  , ''IsFire
                  , ''IsPortal
                  , ''IsPotion
                  , ''IsGoal
                  , ''IsEvil
                  , ''IsPlayer]

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

existsBoxed :: Entity -> ComponentBox -> RogueM Bool
existsBoxed e (C (_ :: c)) = exists e (Proxy :: Proxy c)

-- TODO proxy might serve this purpose better
pl :: a
pl = error "This is a placeholder; it shouldn't have been evaluated!"


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


-- map

type EntityGrid = Matrix (Maybe Entity)

emptyMap :: EntityGrid
emptyMap = (Matrix . FVec.replicate . FVec.replicate) Nothing

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

mkEntityOnEmpty_ :: [ComponentBox] -> RogueM ()
mkEntityOnEmpty_ ss = do
  (x, y) <- randomCoord
  entityExistsAt x y >>= \occupied -> if occupied
    then mkEntityOnEmpty_ ss
    else mkEntity_ $ C (HasLocation x y) : ss


-- comp gen
-----------

randomItemComponents :: RogueM [ComponentBox]
randomItemComponents = do
  regenAmount <- lift $ getRandomR (3, 7)
  component   <- lift
    $ randomChoice (C IsFire : replicate 3 (C $ IsPotion regenAmount))
  return [component]


-- commands
-----------

data Command = Noop | Move (Int, Int) | Drink


-- initialization
-----------------

mkWalls :: RogueM ()
mkWalls = mkWalls' 25 where
  mkWalls' :: Int -> RogueM ()
  mkWalls' 0 = return ()
  mkWalls' n = do
    (x, y) <- randomCoordNoEdge

    let pos1 = if even n then posX else posY
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
              (e, loc) : _ -> existsBoxed e (C IsDoor) >>= \isDoor ->
                pure $ if isDoor then Nothing else (Just . pos1) loc
            )
            .   (if isMin then reverse else id)
            .   sortOn (pos1 . snd)
            .   filter ((if isMin then (>=) else (<=)) dim1 . pos1 . snd)
            .   filter ((== dim2) . pos2 . snd)
            <=< mapM get

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

populateWorld :: RogueM ()
populateWorld = do
  mkWalls
  replicateM_ 5 $ mkEntityOnEmpty_ [C CanMove, C (HasHealth 3), C IsEvil]
  replicateM_ 3 $ randomItemComponents >>= mkEntityOnEmpty_
  forM_ [C (IsPortal In), C (IsPortal Out)] $ mkEntityOnEmpty_ . (: [])
  mkEntityOnEmpty_ [C IsGoal]
  mkEntityOnEmpty_ [C CanMove, C (HasHealth 10), C IsPlayer]


-- movement
-----------

getNeighbors :: (Int, Int) -> RogueM (HashSet (Int, Int))
getNeighbors (x, y) =
  filterM (fmap not . uncurry wallExistsAt)
          [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
    <&> fromList

portalReplace :: (Int, Int) -> RogueM (Int, Int)
portalReplace (x, y) = do
  portalAt <- cfold
    (\found (HasLocation x' y', IsPortal p) ->
      found || (p == In && (x, y) == (x', y'))
    )
    False
  (if portalAt
      then do
        cfold
          (\loc (HasLocation x' y', IsPortal p) ->
            if p == Out then (x', y') else loc
          )
      else pure
    )
    (x, y)

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
          | GoalTile
          | PlayerTile
          | ErrorTile
          deriving Eq


-- systems
----------

data System = System
  { qualifier :: [ComponentBox]
  , action    :: Command -> Entity -> RogueM ()
  , transRepr :: Entity -> Tile -> RogueM Tile
  , transName :: Entity -> Text -> RogueM Text
  }

instance Default System where
  def = System { qualifier = []
               , action    = \_ _ -> pure ()
               , transRepr = const pure
               , transName = const pure
               }

qualified :: Entity -> [ComponentBox] -> RogueM Bool
qualified = allM . existsBoxed


trivialRender :: ComponentBox -> Tile -> Text -> System
trivialRender comp tile name' = def { qualifier = [comp]
                                    , transRepr = \_ _ -> pure tile
                                    , transName = \_ _ -> pure name'
                                    }

moveEvil :: System
moveEvil = def
  { qualifier = [C (HasLocation pl pl), C CanMove, C IsEvil]
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
  { qualifier = [C IsPlayer]
  , action    = \c e -> case c of
                  Drink -> get e >>= \(HasLocation x y) ->
                    findEntityAt x y (Proxy :: Proxy IsPotion) >>= maybe
                      (pure ())
                      (\e' -> do
                        get e' >>= modify e . withHealth . (+) . unPotion
                        destroy e' (Proxy :: Proxy HasLocation)
                        appendMessage "you feel rejuvenated..."
                      )
                  _ -> pure ()
  }

spreadFire :: System
spreadFire = def
  { qualifier = [C IsFire]
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
  { qualifier = [C (HasHealth pl)]
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

systems :: [System]
systems =
  [ -- rendering and naming
    trivialRender (C IsWall)        WallTile   "a wall"
  , trivialRender (C IsDoor)        DoorTile   "a door"
  , trivialRender (C IsEvil)        EvilTile   "a rat"
  , trivialRender (C IsFire)        FireTile   "fire"
  , trivialRender (C $ IsPotion pl) PotionTile "a potion"
  , def { qualifier = [C (IsPortal pl)]
        , transRepr = \e _ -> get e <&> PortalTile . portalType
        , transName = \_ _ -> pure "a portal"
        }
  , trivialRender (C IsGoal)   GoalTile   "the goal"
  , trivialRender (C IsPlayer) PlayerTile "the player"
    -- active
  , def
    { qualifier = [C (HasLocation pl pl), C CanMove, C IsPlayer]
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
    { qualifier = [C (HasHealth pl)]
    , action    = \_ e -> get e >>= \(HasHealth x) ->
                    when (x <= 0)
                    -- no way to just delete an entity entirely?
                      $   destroy
                            e
                            (Proxy :: Proxy (CanMove, HasHealth, HasLocation))
                      >>  name e
                      >>= appendMessage
                      .   (<> " has died!")
    }
    -- message
  , def
    { qualifier = [C (HasHealth pl), C IsPlayer]
    , action    = \_ e ->
                    get e
                      >>= appendMessage
                      .   (\n -> "you have " <> show n <> " hp")
                      .   unHealth
    }
  , def
    { qualifier = [C (HasLocation pl pl), C IsPlayer]
    , action    = \_ e -> do
                    playerLoc <- get e
                    goalLoc   <- cfold
                      (\_ (loc :: HasLocation, IsGoal) -> Just loc)
                      Nothing
                    when (playerLoc == goalLoc) $ appendMessage "you win!"
    }
  , def
    { qualifier = [C IsPlayer]
    , action    = \_ _ -> do
                    addRandomMessage <- (< (0.1 :: Double)) <$> lift getRandom
                    when addRandomMessage
                      $   (lift . randomChoice)
                            ["placeholder msg 1", "placeholder msg 2"]
                      >>= appendMessage
    }
  ]


-- global systems
-----------------

displayIntro :: Command -> RogueM ()
displayIntro _ = get global >>= \case
  (InGameStage Intro, TurnsElapsed 1) -> do
    appendMessage "[enter main game message]"
    populateWorld
    set global $ InGameStage MainGame
  (InGameStage Intro, _) -> appendMessage "[intro message]"
  _                      -> pure ()

incrementTurns :: Command -> RogueM ()
incrementTurns _ = modify global $ withTurnsElapsed succ

globalSystems :: [Command -> RogueM ()]
globalSystems = [displayIntro, incrementTurns]


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


getGrid :: RogueM EntityGrid
getGrid =
  (members (Proxy :: Proxy HasLocation) >>= mapM get) <&> foldl' go emptyMap
 where
  go grid (entity, entityLoc) =
    mset (posX entityLoc) (posY entityLoc) (Just entity) grid

executeStep :: Command -> RogueM ()
executeStep command = do
  set global $ Message []
  forM_ globalSystems (\f -> f command)
  forM_
    systems
    (\s -> do
      members' <- mapM (\(C (_ :: c)) -> members (Proxy :: Proxy c))
                       (qualifier s)
      mapM_ (\e -> whenM (qualified e $ qualifier s) $ action s command e)
            (foldl' union [] members')
    )
  modify global (withMessage reverse)

step
  :: Command
  -> (EntityGrid -> RogueM a)
  -> (Message -> RogueM b)
  -> RogueM (a, b)
step command renderGrid renderMessage = do
  executeStep command
  grid    <- getGrid >>= renderGrid
  message <- get global >>= renderMessage
  pure (grid, message)

runRogue :: RogueM a -> IO a
runRogue f = do
  rng   <- newStdGen
  world <- initWorld
  -- runRandT (runSystem (populateWorld >> f) world) rng <&> fst
  runRandT (runSystem f world) rng <&> fst
