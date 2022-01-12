{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
  ( mkEntityGrid
  , mkGameState
  , step
  , Entity
  , EntityGrid
  , Command(..)
  , GameState(..)
  , Tile(..)
  , getMessage
  , represent
  , setCommand
  , isDoor
  , isWall
  ) where

import           Relude

import           Utils

import           Control.Lens
import           Control.Monad.Random    hiding ( fromList )
import           Data.Default
import           Data.Graph.AStar
import           Data.List                      ( delete )
import           Data.Maybe
import qualified Data.Set                      as Set
import qualified Data.Vector.Fixed             as Vec
import qualified GHC.Exts


data Component =
    CanMove
  | HasHealth Int
  | IsWall
  | IsDoor
  | IsFire
  | IsPotion Effect
  | IsPortal Portal
  | IsGoal
  | IsEvil
  | IsPlayer
  deriving (Eq, Ord, Show)

data Effect = NoEffect | RegenerateEffect Int
  deriving (Eq, Ord, Show)

data Portal = In | Out deriving (Eq, Ord, Show)

-- TODO location could be a component!
--      this would allow for eg inventory items
data Entity = Entity
  { _posX       :: Int
  , _posY       :: Int
  , _components :: Set Component
  }
  deriving (Eq, Show)

makeLenses ''Entity

-- TODO use th or something?
canMove, hasHealth, isPlayer, isGoal, isWall, isDoor, isEvil, isFire, isPotion, isPortal
  :: Entity -> Bool
canMove =
  any
      (\case
        CanMove -> True
        _       -> False
      )
    . view components
hasHealth =
  any
      (\case
        HasHealth _ -> True
        _           -> False
      )
    . view components
isPlayer =
  any
      (\case
        IsPlayer -> True
        _        -> False
      )
    . view components
isGoal =
  any
      (\case
        IsGoal -> True
        _      -> False
      )
    . view components
isWall =
  any
      (\case
        IsWall -> True
        _      -> False
      )
    . view components
isDoor =
  any
      (\case
        IsDoor -> True
        _      -> False
      )
    . view components
isEvil =
  any
      (\case
        IsEvil -> True
        _      -> False
      )
    . view components
isFire =
  any
      (\case
        IsFire -> True
        _      -> False
      )
    . view components
isPotion =
  any
      (\case
        IsPotion _ -> True
        _          -> False
      )
    . view components
isPortal =
  any
      (\case
        IsPortal _ -> True
        _          -> False
      )
    . view components

getHealth :: Entity -> Int
getHealth e =
  (\case
      h : _ -> h
      []    -> error $ "Called getHealth on entity without health: " <> show e
    )
    . concatMap
        (\case
          HasHealth h -> [h]
          _           -> []
        )
    . view components
    $ e
setHealth :: Int -> Entity -> Entity
setHealth n = over
  components
  (Set.map
    (\case
      HasHealth _ -> HasHealth n
      c           -> c
    )
  )

getEffect :: Entity -> Effect
getEffect e =
  (\case
      h : _ -> h
      []    -> error $ "Called getHealth on non-potion: " <> show e
    )
    . concatMap
        (\case
          IsPotion f -> [f]
          _          -> []
        )
    . view components
    $ e

getPortal :: Entity -> Portal
getPortal e =
  (\case
      h : _ -> h
      []    -> error $ "Called getPortal on non-portal: " <> show e
    )
    . concatMap
        (\case
          IsPortal p -> [p]
          _          -> []
        )
    . view components
    $ e

randomItemComponents :: RandM (Set Component)
randomItemComponents = do
  regenerateAmt <- getRandomR (3, 7)
  effect        <- randomChoice [RegenerateEffect regenerateAmt]
  component     <- randomChoice (IsFire : replicate 3 (IsPotion effect))
  (return . fromList) [component]


type EntityGrid = Matrix (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Matrix . Vec.replicate . Vec.replicate $ Nothing


data Command = Noop | North | East | South | West | Drink deriving (Eq, Ord, Show)


findCoords :: Foldable t => (Entity -> Bool) -> t Entity -> (Int, Int)
findCoords f = getCoords . fromJust . find f

getCoords :: Entity -> (Int, Int)
getCoords e = (e ^. posX, e ^. posY)


data GameState = GameState
  { _entities :: [Entity]
  , _command  :: Command
  , _message  :: [Text]
  }

makeLenses ''GameState

getMessage :: GameState -> [Text]
getMessage = view message

setCommand :: Command -> GameState -> GameState
setCommand = set command

mkGameState :: Command -> RandM GameState
mkGameState cmd = do
  entities' <-
    (mkPlayer <=< mkGoal <=< mkPortals <=< mkItems <=< mkEvil <=< mkWalls) []
  let gameState =
        GameState { _entities = entities', _command = cmd, _message = [] }
  case
      gridAStar entities'
                (findCoords isPlayer entities')
                (findCoords isGoal entities')
    of
      Just p  -> if length p >= 14 then return gameState else mkGameState cmd
      Nothing -> mkGameState cmd

 where
  randomCoord       = getRandomR (0 :: Int, matrixSize - 1)
  randomCoordNoEdge = getRandomR (1 :: Int, matrixSize - 2)

  mkEntityOnEmpty comps es = do
    coords <- replicateM 2 randomCoord
    let [x, y] :: [Int] = case coords of
          x' : y' : _ -> [x', y']
          _           -> error "???"

    if any (\e -> e ^. posX == x && e ^. posY == y) es
      then mkEntityOnEmpty comps es
      else return $ Entity x y comps : es

  mkPlayer  = mkEntityOnEmpty [CanMove, HasHealth 10, IsPlayer]

  mkGoal    = mkEntityOnEmpty [IsGoal]

  mkPortals = mkEntityOnEmpty [IsPortal In] >=> mkEntityOnEmpty [IsPortal Out]

  mkItems   = mkItems' (5 :: Integer)
  mkItems' 0 es = return es
  mkItems' n es = do
    cs <- randomItemComponents
    mkEntityOnEmpty cs es >>= mkItems' (n - 1)

  mkEvil = mkEvil' (8 :: Integer)
  mkEvil' 0 es = return es
  mkEvil' n es =
    mkEntityOnEmpty [CanMove, HasHealth 3, IsEvil] es >>= mkEvil' (n - 1)

  mkWalls es = mkWalls' (25 :: Integer) es
  -- TODO code is ugly; should probably refactor slightly
  mkWalls' 0 es = return es
  mkWalls' n es = do
    coords <- replicateM 2 randomCoordNoEdge
    let
      -- no MonadFail instance!
      [x, y] :: [Int] = case coords of
        x' : y' : _ -> [x', y']
        _           -> error "???"
      -- x and y are reversed depending on whether the wall is vertical or
      -- horizontal
      pos1 isX = if isX then posX else posY
      dim1 isX = if isX then x else y
      pos2 isX = if isX then posY else posX
      dim2 isX = if isX then y else x

      adjEntities isX = filter
        (\e ->
          abs (view (pos1 isX) e - dim1 isX)
            <  3
            && view (pos2 isX) e
            -  dim2 isX
            == 0
        )
        es
      constrainWalls isMin isX =
        (\case
            []    -> if isMin then Just 0 else Just (matrixSize - 1)
            e : _ -> if Set.member IsDoor (e ^. components)
              then Nothing
              else Just $ e ^. pos1 isX
          )
          . (if isMin then reverse else id)
          . sortOn (view $ pos1 isX)
          . filter ((if isMin then (>=) else (<=)) (dim1 isX) . view (pos1 isX))
          . filter ((== dim2 isX) . view (pos2 isX))
      dimRanges =
        [ constrainWalls isMin isX es
        | isX   <- [True, False]
        , isMin <- [True, False]
        ]

    case dimRanges of
      [Just minX, Just maxX, Just minY, Just maxY] -> do
        let newWalls = if even n
              then [ Entity x' y [IsWall] | x' <- [minX .. maxX] ]
              else [ Entity x y' [IsWall] | y' <- [minY .. maxY] ]
        doorPos <- getRandomR (0, length newWalls - 1)
        let newWallsWithDoor = newWalls & ix doorPos %~ set components [IsDoor]
        if null (adjEntities $ odd n)
          then mkWalls' (n + 1) (newWallsWithDoor ++ es)
          else mkWalls' (n - 1) es
      _ -> mkWalls' (n - 1) es


-- types of tiles that the renderer should use
-- (corresponding representations/images aren't actually provided in Game.hs)
data Tile = DefaultTile
          | PlayerTile
          | GoalTile
          | WallTile
          | DoorTile
          | EvilTile
          | FireTile
          | PotionTile
          | InPortalTile
          | OutPortalTile

data System = System
  { qualifier :: Entity -> Bool
  , everyTick :: Command -> Entity -> GameState -> RandM GameState
  , buildRepr :: Entity -> Tile -> Tile
  , buildName :: Entity -> Text -> Text
  }

instance Default System where
  def = System { qualifier = const False
               , everyTick = \_ _ -> pure
               , buildRepr = const id
               , buildName = const id
               }

-- runs a system's everyTick over all entities
runSystemET :: System -> GameState -> RandM GameState
runSystemET s gs = (foldl' (<=<) pure . map (everyTick s $ gs ^. command))
  (filter (qualifier s) (gs ^. entities))
  gs

-- evil autogenerated type sig
getNeighbors
  :: (Foldable f, IsList c, GHC.Exts.Item c ~ (Int, Int))
  => f Entity
  -> (Int, Int)
  -> c
getNeighbors es (x, y) =
  ( fromList
    . filter
        (\(x', y') ->
          none (\e -> e ^. posX == x' && e ^. posY == y' && isWall e) es
            && withinBounds x'
            && withinBounds y'
        )
    . map givenPortal
    )
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
 where
  withinBounds = (>= 0) &&$ (< matrixSize)
  givenPortal (x', y') = if maybe False isPortal (toEntity x' y')
    then maybe (x', y') fromEntity
      $ find (isPortal &&$ ((== Out) . getPortal)) es
    else (x', y')
  fromEntity e = (e ^. posX, e ^. posY)
  toEntity x' y' = find (((== x') . view posX) &&$ ((== y') . view posY)) es

-- a* for our entity list
gridAStar :: [Entity] -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
gridAStar es begin dest =
  aStar (getNeighbors es) (\_ _ -> 1 :: Integer) (const 1) (== dest) begin

-- this has to prevent entities from walking on certain things
attemptMove :: Int -> Int -> Entity -> GameState -> GameState
attemptMove x y e g =
  (case filter (hasHealth &&$ sameLoc) (g ^. entities) of
      e' : _ ->
        over message (name e <> " viciously attacks " <> name e' :)
          . over entities (replace e' (setHealth (getHealth e' - 1) e'))
      [] -> if x < 0 || x >= matrixSize || y < 0 || y >= matrixSize
        then over message ("beyond the map lie unspeakable horrors." :)
          . over entities (replace' (== e) (setHealth (-666)))
        else over
          entities
          (\es -> if any (isWall &&$ sameLoc) es
            then es
            else replace e (set posY y . set posX x $ e) es
          )
    )
    g
  where sameLoc e' = (e' ^. posX, e' ^. posY) == (x, y)

fromMoveCommand :: Command -> Entity -> GameState -> GameState
fromMoveCommand c e =
  (case c of
      North -> attemptMove (e ^. posX) (e ^. posY - 1)
      East  -> attemptMove (e ^. posX + 1) (e ^. posY)
      South -> attemptMove (e ^. posX) (e ^. posY + 1)
      West  -> attemptMove (e ^. posX - 1) (e ^. posY)
      _     -> \_ x -> x
    )
    e

spreadFire :: System
spreadFire = def
  { qualifier = isFire
  , everyTick = \_ e g -> do
    neighbor <- randomChoice $ getNeighbors (g ^. entities) (getCoords e)
    let entity = Entity { _posX       = fst neighbor
                        , _posY       = snd neighbor
                        , _components = [IsFire]
                        }
    getRandom
      <&> \r -> if r > (0.2 :: Double) then g else over entities (entity :) g
  }

detectWin :: System
detectWin = def
  { qualifier = isPlayer
  , everyTick = \_ e g ->
    pure
      $ if getCoords e == (getCoords . fromJust . find isGoal) (g ^. entities)
          then over message ("you win!" :) g
          else g
  }

moveEvil :: System
moveEvil = def
  { qualifier = canMove &&$ isEvil
  , everyTick = \_ e g ->
                  pure
                    $ (case filter isPlayer $ g ^. entities of
                        p : _ ->
                          (case
                              gridAStar (g ^. entities)
                                        (e ^. posX, e ^. posY)
                                        ((\e' -> (e' ^. posX, e' ^. posY)) p)
                            of
                              Just ((x, y) : _) -> attemptMove x y e
                              _                 -> id
                          )
                        [] -> id
                      )
                        g
  }

detectFire :: System
detectFire = def
  { qualifier = isPlayer
  , everyTick = \_ e g ->
    pure $ if any (isFire &&$ ((== getCoords e) . getCoords)) (g ^. entities)
      then over entities (replace e (setHealth (getHealth e - 1) e)) g
      else g
  }

applyPortal :: System
applyPortal = def
  { qualifier = canMove
  , everyTick = \_ e g ->
                  pure
                    $ case
                        ( find
                          (   isPortal
                          &&$ ((== In) . getPortal)
                          &&$ ((== view posX e) . view posX)
                          &&$ ((== view posY e) . view posY)
                          )
                          (g ^. entities)
                        , find (isPortal &&$ ((== Out) . getPortal))
                               (g ^. entities)
                        )
                      of
                        (Just _, Just o) ->
                          over message (name e <> " has teleported!" :)
                            . over
                                entities
                                (  replace e
                                $  e
                                &  posX
                                .~ (o ^. posX)
                                &  posY
                                .~ (o ^. posY)
                                )
                            $ g
                        _ -> g
  }

drinkPotion :: System
drinkPotion = def
  { qualifier = isPlayer
  , everyTick = \c e g -> pure $ if c == Drink
    then
      case
        find
          (\e' ->
            e ^. posX == e' ^. posX && e ^. posY == e' ^. posY && isPotion e'
          )
          (g ^. entities)
      of
        Just p  -> over entities (delete p) . applyEffect p $ g
        Nothing -> g -- drinking nothing doesn't do very much
    else g
  }
 where
  applyEffect p = over
    entities
    (replace' isPlayer $ case getEffect p of
      RegenerateEffect n -> \e -> setHealth (getHealth e + n) e
      NoEffect           -> id
    )

systems :: [System]
systems =
  [ -- render and name player
    def { qualifier = isPlayer
        , buildRepr = \_ _ -> PlayerTile
        , buildName = \_ _ -> "the player"
        }
    -- render goal
  , def { qualifier = isGoal, buildRepr = \_ _ -> GoalTile }
    -- render wall
  , def { qualifier = isWall, buildRepr = \_ _ -> WallTile }
    -- render door
  , def { qualifier = isDoor, buildRepr = \_ _ -> DoorTile }
    -- render and name enemy
  , def { qualifier = isEvil
        , buildRepr = \_ _ -> EvilTile
        , buildName = \_ _ -> "a rat"
        }
    -- render fire
  , def { qualifier = isFire, buildRepr = \_ _ -> FireTile }
    -- render potion
  , def { qualifier = isPotion, buildRepr = \_ _ -> PotionTile }
    -- render in portal
  , def { qualifier = isPortal &&$ ((== In) . getPortal)
        , buildRepr = \_ _ -> InPortalTile
        }
    -- render out portal
  , def { qualifier = isPortal &&$ ((== Out) . getPortal)
        , buildRepr = \_ _ -> OutPortalTile
        }
    -- spread fire
  , spreadFire
    -- move player
  , def { qualifier = isPlayer &&$ canMove
        , everyTick = \c e g -> pure $ fromMoveCommand c e g
        }
    -- move enemy
  , moveEvil
    -- detect fire
  , detectFire
    -- apply portal
  , applyPortal
    -- drink potion
  , drinkPotion
    -- display player hp
  , def
    { qualifier = isPlayer &&$ hasHealth
    , everyTick = \_ e g -> pure $ over
                    message
                    (++ ["you have " <> (show . getHealth) e <> " hp"])
                    g
    }
    -- implement death
  , def
    { qualifier = hasHealth
    , everyTick = \_ e -> pure . if getHealth e <= 0
                    then over message (name e <> " has died!" :)
                      . over entities (delete e)
                    else id
    }
  , -- implement goal
    detectWin
  ]

buildFromEntity :: (System -> Entity -> a -> a) -> a -> Entity -> a
buildFromEntity f x e =
  (foldl' (.) id . map (($ e) . f) . filter (($ e) . qualifier) $ systems) x

name :: Entity -> Text
name = buildFromEntity buildName "something"

represent :: Entity -> Tile
represent = buildFromEntity buildRepr DefaultTile


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid =
  flip compose mkEntityGrid
    . map setEntity
    . sortOn (Down . view components)
    . (^. entities)
  where setEntity e = mset (e ^. posX) (e ^. posY) (Just e)

executeStep :: GameState -> RandM GameState
executeStep gs =
  foldr ((<=<) . runSystemET) pure systems (set message [] gs)
    <&> over message reverse

step :: (EntityGrid -> a) -> StateT GameState RandM a
step render = do
  get >>= lift . executeStep >>= put
  get <&> render . getGrid
