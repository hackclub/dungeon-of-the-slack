{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game
  ( mkEntityGrid
  , mkGameState
  , components
  , isDoor
  , isWall
  , Entity
  , EntityGrid
  , Command(..)
  , GameState
  , Tile(..)
  , represent
  , setCommand
  , step
  ) where

import           Relude

import           Utils

import           Control.Lens
import           Data.Default
import qualified Data.Set                      as Set
import qualified Data.Vector.Fixed             as Vec
import           System.Random


data Component = CanMove | IsPlayer | IsWall | IsDoor | IsPotion Effect deriving (Eq, Ord)

data Effect = Effect
  deriving (Eq, Ord)

-- TODO use th or something?
canMove :: Set Component -> Bool
canMove = any
  (\case
    CanMove -> True
    _       -> False
  )
isPlayer :: Set Component -> Bool
isPlayer = any
  (\case
    IsPlayer -> True
    _        -> False
  )
isWall :: Set Component -> Bool
isWall = any
  (\case
    IsWall -> True
    _      -> False
  )
isDoor :: Set Component -> Bool
isDoor = any
  (\case
    IsDoor -> True
    _      -> False
  )
isPotion :: Set Component -> Bool
isPotion = any
  (\case
    IsPotion _ -> True
    _          -> False
  )

randomItemComponents :: StdGen -> Set Component
randomItemComponents rng =
  fromList [randomChoice [IsPotion (randomChoice [Effect] rng)] rng]


data Entity = Entity
  { _posX       :: Int
  , _posY       :: Int
  , _components :: Set Component
  }
  deriving Eq

makeLenses ''Entity


type EntityGrid = Matrix (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Matrix . Vec.replicate . Vec.replicate $ Nothing


data Command = Noop | North | East | South | West deriving (Eq, Ord, Show)


data GameState = GameState
  { gameRNG   :: StdGen
  , _entities :: [Entity]
  , _command  :: Command
  }

makeLenses ''GameState

mkGameState :: Command -> IO GameState
mkGameState cmd = do
  rng <- getStdGen
  -- lmao
  let (rng', (rngWalls, (rngPlayer, rngItems))) =
        (fmap (fmap split . split) . split) rng
  return $ GameState
    { gameRNG   = rng'
    , _entities = (mkPlayer rngPlayer . mkItems rngItems . mkWalls rngWalls) []
    , _command  = cmd
    }
 where
  randomCoord       = randomR (0 :: Int, matrixSize - 1)
  randomCoordNoEdge = randomR (1 :: Int, matrixSize - 2)

  mkEntityOnEmpty comps rng es =
    if any (\e -> e ^. posX == x && e ^. posY == y) es
      then mkEntityOnEmpty comps newRNG es
      else Entity x y comps : es
   where
    (x, rng'  ) = randomCoord rng
    (y, newRNG) = randomCoord rng'

  mkPlayer = mkEntityOnEmpty [CanMove, IsPlayer]
  mkItems  = mkItems' (10 :: Integer)
  mkItems' 0 _ es = es
  mkItems' n rng es =
    let (compsRNG, newRNG) = split rng
        es' = mkEntityOnEmpty (randomItemComponents compsRNG) rng es
    in  mkItems' (n - 1) newRNG es'

  mkWalls rng es = mkWalls' (25 :: Integer) rng es

  -- TODO code is ugly; should probably refactor slightly
  mkWalls' 0 _ es = es
  mkWalls' n rng es =
    let
      -- x and y are reversed depending on whether the wall is vertical or
      -- horizontal
      pos1 isX = if isX then posX else posY
      dim1 isX = if isX then x else y
      pos2 isX = if isX then posY else posX
      dim2 isX = if isX then y else x

      (x, rng' ) = randomCoordNoEdge rng
      (y, rng'') = randomCoordNoEdge rng'
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
    in
      case dimRanges of
        [Just minX, Just maxX, Just minY, Just maxY] ->
          let
            newWalls = if even n
              then [ Entity x' y [IsWall] | x' <- [minX .. maxX] ]
              else [ Entity x y' [IsWall] | y' <- [minY .. maxY] ]
            (doorPos, newRNG) = randomR (0, length newWalls - 1) rng''
            newWallsWithDoor  = newWalls & ix doorPos %~ set components [IsDoor]
          in
            if null (adjEntities $ odd n)
              then mkWalls' (n + 1) newRNG (newWallsWithDoor ++ es)
              else mkWalls' (n - 1) newRNG es
        _ -> mkWalls' (n - 1) rng'' es


setCommand :: Command -> GameState -> GameState
setCommand = set command


data Tile = DefaultTile | PlayerTile | WallTile | DoorTile | PotionTile deriving Eq

data System = System
  { qualifier :: Entity -> Bool
  , everyTick :: Command -> Entity -> GameState -> GameState
  , buildRepr :: Entity -> Tile -> Tile
  }

instance Default System where
  def = System { qualifier = const False
               , everyTick = \_ _ -> id
               , buildRepr = const id
               }

-- runs a system's everyTick over all entities
runSystemET :: System -> GameState -> GameState
runSystemET s gs = (foldl' (.) id . map (everyTick s $ gs ^. command))
  (filter (qualifier s) (gs ^. entities))
  gs

systems :: [System]
systems =
  [ -- render player
    def { qualifier = isPlayer . view components
        , buildRepr = \_ _ -> PlayerTile
        }
    -- move player
  , def
    { qualifier = (\cs -> isPlayer cs && canMove cs) . view components
    , everyTick = \c e g ->
      (over entities $ replace e $ fromMoveCommand c (g ^. entities) e) g
    }
    -- render wall
  , def { qualifier = isWall . view components, buildRepr = \_ _ -> WallTile }
    -- render door
  , def { qualifier = isDoor . view components, buildRepr = \_ _ -> DoorTile }
    -- render potion
  , def { qualifier = isPotion . view components
        , buildRepr = \_ _ -> PotionTile
        }
  ]
 where
   -- this has to prevent entities from walking on walls
  fromMoveCommand c es e =
    let newEntity = case c of
          North -> over posY (subtract 1) e
          East  -> over posX (+ 1) e
          South -> over posY (+ 1) e
          West  -> over posX (subtract 1) e
          _     -> e
        newPos = (newEntity ^. posX, newEntity ^. posY)
    in  if any
             (\e' ->
               Set.member IsWall (e' ^. components)
                 && ((e' ^. posX, e' ^. posY) == newPos)
             )
             es
          then e
          else newEntity

represent :: Entity -> Tile
represent e =
  ( foldl' (.) id
    . map (($ e) . buildRepr)
    . filter (($ e) . qualifier)
    $ systems
    )
    DefaultTile


-- currently an out-of-bounds entity will just not appear
-- this is bc gset uses ix
getGrid :: GameState -> EntityGrid
getGrid = flip compose mkEntityGrid . map setEntity . (^. entities)
  where setEntity e = mset (e ^. posX) (e ^. posY) (Just e)


executeStep :: GameState -> GameState
executeStep = (compose . map runSystemET) systems

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
