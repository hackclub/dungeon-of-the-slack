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

-- TODO re-add combat, death, potions, portals, fire

module Game.New
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
import qualified Data.Vector.Fixed             as VecF
import qualified Data.Vector.Unboxed           as Vec


type EntityGrid = Matrix (Maybe Entity)


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
  [("potionRegen", mkType ''Int [])]
  ''Map

-- goal

defComponent "IsGoal" [] ''Map

-- alive

defComponent "IsEvil" [] ''Map
defComponent "IsPlayer" [] ''Unique


-- world
--------

makeWorld "World" [ ''Message
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

type Gettable a = Get World RandIOM a
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

emptyMap :: EntityGrid
emptyMap = (Matrix . VecF.replicate . VecF.replicate) Nothing

maxCoord :: Int
maxCoord = matrixSize - 1


-- message

appendMessage :: Text -> RogueM ()
appendMessage text = modify global $ withMessage (text :)


-- commands
-----------

data Command = Noop | Move (Int, Int) | Drink


-- location
-----------

fromLocation :: HasLocation -> (Int, Int)
fromLocation loc = (posX loc, posY loc)

entityExistsAt :: Int -> Int -> RogueM Bool
entityExistsAt x y =
  cfold (\found comp -> found || fromLocation comp == (x, y)) False

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


-- initialization
-----------------

mkWalls :: RogueM ()
mkWalls = mkWalls' 25 where
  mkWalls' :: Int -> RogueM ()
  mkWalls' 0 = return ()
  mkWalls' n = do
    (x, y) <- randomCoordNoEdge

    let pos1 isX = if isX then posX else posY
        dim1 isX = if isX then x else y
        pos2 isX = if isX then posY else posX
        dim2 isX = if isX then y else x

        adjEntityLocs :: Bool -> RogueM [HasLocation]
        adjEntityLocs isX = cfold
          (\ls loc ->
            if abs (pos1 isX loc - dim1 isX) < 3 && pos2 isX loc - dim2 isX == 0
              then loc : ls
              else ls
          )
          []

        constrainWalls isMin isX =
          (\case
              [] -> pure $ if isMin then Just 0 else Just (matrixSize - 1)
              (e, loc) : _ -> existsBoxed e (C IsDoor) >>= \isDoor ->
                pure $ if isDoor then Nothing else (Just . pos1 isX) loc
            )
            .   (if isMin then reverse else id)
            .   sortOn (pos1 isX . snd)
            .   filter
                  ((if isMin then (>=) else (<=)) (dim1 isX) . pos1 isX . snd)
            .   filter ((== dim2 isX) . pos2 isX . snd)
            <=< mapM get

    entities  <- members (Proxy :: Proxy HasLocation)
    dimRanges <- sequence
      [ constrainWalls isMin isX entities
      | isX   <- [True, False]
      , isMin <- [True, False]
      ]

    valid <- adjEntityLocs (odd n) <&> null
    if valid
      then case dimRanges of
        [Just minX, Just maxX, Just minY, Just maxY] -> do
          newWalls <- sequence $ if even n
            then
              [ mkEntity [C (HasLocation x' y), C IsWall]
              | x' <- [minX .. maxX]
              ]
            else
              [ mkEntity [C (HasLocation x y'), C IsWall]
              | y' <- [minY .. maxY]
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
  replicateM_ 5 $ randomItemComponents >>= mkEntityOnEmpty_
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

pathfind :: (Int, Int) -> (Int, Int) -> RogueM (Maybe [(Int, Int)])
pathfind begin dest = aStarM getNeighbors
                             (\_ _ -> pure (1 :: Int))
                             (const $ pure 1)
                             (pure . (== dest))
                             (pure begin)

moveTo :: Entity -> Int -> Int -> RogueM ()
moveTo entity destX destY = do
  HasLocation curX curY <- get entity
  getNeighbors (curX, curY)
    >>= flip when (set entity $ HasLocation destX destY)
    .   ((destX, destY) `elem`)
    .   toList


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
  }

instance Default System where
  def =
    System { qualifier = [], action = \_ _ -> pure (), transRepr = const pure }

qualified :: Entity -> [ComponentBox] -> RogueM Bool
qualified = allM . existsBoxed


trivialRender :: ComponentBox -> Tile -> System
trivialRender comp tile =
  def { qualifier = [comp], transRepr = \_ _ -> pure tile }

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

systems :: [System]
systems =
  [ -- rendering
    trivialRender (C IsWall)        WallTile
  , trivialRender (C IsDoor)        DoorTile
  , trivialRender (C IsEvil)        EvilTile
  , trivialRender (C IsFire)        FireTile
  , trivialRender (C $ IsPotion pl) PotionTile
  , def { qualifier = [C (IsPortal pl)]
        , transRepr = \e _ -> get e <&> PortalTile . portalType
        }
  , trivialRender (C IsGoal)   GoalTile
  , trivialRender (C IsPlayer) PlayerTile
    -- action
  , def
    { qualifier = [C (HasLocation pl pl), C CanMove, C IsPlayer]
    , action    = \c e -> case c of
                    Move (x, y) -> do
                      HasLocation x' y' <- get e
                      moveTo e (x + x') (y + y')
                    _ -> pure ()
    }
  , moveEvil
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
  ]


-- run
-------

represent :: Entity -> RogueM Tile
represent entity = foldl'
  (>=>)
  (const $ pure ErrorTile)
  (map (\s -> whenQualified s . (entity &) . transRepr $ s) systems)
  entity
 where
  whenQualified system m tile = qualified entity (qualifier system)
    >>= \qual -> (if qual then m else pure) tile

getGrid :: RogueM EntityGrid
getGrid =
  (members (Proxy :: Proxy HasLocation) >>= mapM get) <&> foldl' go emptyMap
 where
  go grid (entity, entityLoc) =
    mset (posX entityLoc) (posY entityLoc) (Just entity) grid

executeStep :: Command -> RogueM ()
executeStep command = do
  set global $ Message []
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
  runRandT (runSystem (populateWorld >> f) world) rng <&> fst
