{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

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

import           Control.Monad.Random
import           Data.Default
import qualified Data.Vector.Fixed             as VecF
import qualified Data.Vector.Unboxed           as Vec
import           Relude.Unsafe


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
data GetBox = forall a . Gettable a => G a

existsBoxed :: Entity -> GetBox -> RogueM Bool
existsBoxed e (G (_ :: c)) = exists e (Proxy :: Proxy c)

-- TODO proxy might serve this purpose better
pl :: a
pl = error "This is a placeholder; it shouldn't have been evaluated!"


type Settable a = Set World RandIOM a
data SetBox = forall a . Settable a => S a

setBoxed :: Entity -> SetBox -> RogueM ()
setBoxed e (S c) = set e c

mkEntity :: [SetBox] -> RogueM Entity
mkEntity = (nextEntity >>=) . mkEntity'
 where
  mkEntity' (s : ss) e = setBoxed e s >> mkEntity' ss e
  mkEntity' []       e = pure e

mkEntity_ :: [SetBox] -> RogueM ()
mkEntity_ = void . mkEntity


-- members

type Memberable a = Members World RandIOM a

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

mkEntityOnEmpty_ :: [SetBox] -> RogueM ()
mkEntityOnEmpty_ ss = do
  (x, y) <- randomCoord
  entityExistsAt x y >>= \occupied -> if occupied
    then mkEntityOnEmpty_ ss
    else mkEntity_ $ S (HasLocation x y) : ss


-- comp gen
-----------

randomItemComponents :: RogueM [SetBox]
randomItemComponents = do
  regenAmount <- lift $ getRandomR (3, 7)
  component   <- lift
    $ randomChoice (S IsFire : replicate 3 (S $ IsPotion regenAmount))
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
              (e, loc) : _ -> existsBoxed e (G IsDoor) >>= \isDoor ->
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
              [ mkEntity [S (HasLocation x' y), S IsWall]
              | x' <- [minX .. maxX]
              ]
            else
              [ mkEntity [S (HasLocation x y'), S IsWall]
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
  replicateM_ 5 $ mkEntityOnEmpty_ [S CanMove, S (HasHealth 3), S IsEvil]
  replicateM_ 5 $ randomItemComponents >>= mkEntityOnEmpty_
  forM_ [S (IsPortal In), S (IsPortal Out)] $ mkEntityOnEmpty_ . (: [])
  mkEntityOnEmpty_ [S IsGoal]
  mkEntityOnEmpty_ [S CanMove, S (HasHealth 10), S IsPlayer]


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
  { qualifier :: [GetBox]
  , action    :: Command -> Entity -> RogueM ()
  , transRepr :: Entity -> Tile -> RogueM Tile
  }

instance Default System where
  def =
    System { qualifier = [], action = \_ _ -> pure (), transRepr = const pure }

qualified :: Entity -> [GetBox] -> RogueM Bool
qualified = allM . existsBoxed

systems :: [System]
systems =
  [ -- rendering
    def { qualifier = [G IsWall], transRepr = \_ _ -> pure WallTile }
  , def { qualifier = [G IsDoor], transRepr = \_ _ -> pure DoorTile }
  , def { qualifier = [G IsEvil], transRepr = \_ _ -> pure EvilTile }
  , def { qualifier = [G IsFire], transRepr = \_ _ -> pure FireTile }
  , def { qualifier = [G (IsPotion pl)], transRepr = \_ _ -> pure PotionTile }
  , def { qualifier = [G (IsPortal pl)]
        , transRepr = \e _ -> get e <&> PortalTile . portalType
        }
  , def { qualifier = [G IsGoal], transRepr = \_ _ -> pure GoalTile }
  , def { qualifier = [G IsPlayer], transRepr = \_ _ -> pure PlayerTile }
    -- action
  , def
    { qualifier = [G (HasLocation pl pl), G CanMove, G IsPlayer]
    , action    = \c e -> case c of
      Move (x, y) ->
        modify e (\(HasLocation x' y') -> HasLocation (x + x') (y + y'))
      _ -> pure ()
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
executeStep _ = do
  set global $ Message []
  -- TODO systems
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
