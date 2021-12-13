{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
  ( mkEntityGrid
  , mkGameState
  , components
  , isDoor
  , isWall
  , Entity
  , EntityGrid
  , Command(..)
  , GameState(..)
  , message
  , Tile(..)
  , represent
  , setCommand
  , step
  ) where

import           Prelude                        ( head )
import           Relude                  hiding ( head )

import           Utils

import           Control.Lens
import           Data.Default
import           Data.Graph.AStar
import           Data.List                      ( delete )
import qualified Data.Set                      as Set
import qualified Data.Vector.Fixed             as Vec
import           System.Random


data Component =
    CanMove
  | HasHealth Int
  | IsWall
  | IsDoor
  | IsPotion Effect
  | IsEvil
  | IsPlayer
  deriving (Eq, Ord)

data Effect = Effect
  deriving (Eq, Ord)

data Entity = Entity
  { _posX       :: Int
  , _posY       :: Int
  , _components :: Set Component
  }
  deriving Eq

makeLenses ''Entity

-- TODO use th or something?
canMove :: Set Component -> Bool
canMove = any
  (\case
    CanMove -> True
    _       -> False
  )
hasHealth :: Set Component -> Bool
hasHealth = any
  (\case
    HasHealth _ -> True
    _           -> False
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
isEvil :: Set Component -> Bool
isEvil = any
  (\case
    IsEvil -> True
    _      -> False
  )
isPotion :: Set Component -> Bool
isPotion = any
  (\case
    IsPotion _ -> True
    _          -> False
  )

getHealth :: Entity -> Int
getHealth =
  head
    . concatMap
        (\case
          HasHealth h -> [h]
          _           -> []
        )
    . view components

setHealth :: Int -> Entity -> Entity
setHealth n = over
  components
  (Set.map
    (\case
      HasHealth _ -> HasHealth n
      c           -> c
    )
  )

randomItemComponents :: StdGen -> Set Component
randomItemComponents rng =
  fromList [randomChoice [IsPotion (randomChoice [Effect] rng)] rng]


type EntityGrid = Matrix (Maybe Entity)

mkEntityGrid :: EntityGrid
mkEntityGrid = Matrix . Vec.replicate . Vec.replicate $ Nothing


data Command = Noop | North | East | South | West | Drink deriving (Eq, Ord, Show)


data GameState = GameState
  { gameRNG   :: StdGen
  , _entities :: [Entity]
  , _command  :: Command
  , _message  :: [Text]
  }

makeLenses ''GameState

mkGameState :: Command -> IO GameState
mkGameState cmd = do
  -- rng <- getStdGen
  -- -- lmao
  -- let (rng', (rngWalls, (rngPlayer, rngItems))) =
  --       ((fmap (fmap split . split) . split)) rng
  [rng, rngWalls, rngPlayer, rngEvil, rngItems] <- replicateM 5 newStdGen
  return $ GameState
    { gameRNG   = rng -- rng'
    , _entities =
      (mkPlayer rngPlayer . mkItems rngItems . mkEvil rngEvil . mkWalls rngWalls
        )
        []
    , _command  = cmd
    , _message  = []
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

  mkPlayer = mkEntityOnEmpty [CanMove, HasHealth 10, IsPlayer]

  mkEvil   = mkEvil' (5 :: Integer)
  mkEvil' 0 _   es = es
  mkEvil' n rng es = mkEvil' (n - 1) newRNG es'
   where
    (newRNG, _) = split rng
    es'         = mkEntityOnEmpty [CanMove, HasHealth 3, IsEvil] rng es

  mkItems = mkItems' (5 :: Integer)
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


data Tile = DefaultTile | PlayerTile | WallTile | DoorTile | EvilTile | PotionTile deriving Eq

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

gridAStar :: [Entity] -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
gridAStar es begin dest = aStar getNeighbors
                                (\_ _ -> 1)
                                (const 1)
                                (== dest)
                                begin
 where
  getNeighbors (x, y) =
    (fromList . filter
        (\(x', y') -> none
          (\e ->
            e ^. posX == x' && e ^. posY == y' && (isWall . view components) e
          )
          es
        )
      )
      [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

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
                    (set entities $ fromMoveCommand c (g ^. entities) e) g
    }
    -- display player hp
  , def
    { qualifier = (\cs -> isPlayer cs && hasHealth cs) . view components
    , everyTick = \_ e g ->
      over message (++ ["you have " <> (show . getHealth) e <> " hp"]) g
    }
    -- render wall
  , def { qualifier = isWall . view components, buildRepr = \_ _ -> WallTile }
    -- render door
  , def { qualifier = isDoor . view components, buildRepr = \_ _ -> DoorTile }
    -- render enemy
  , def { qualifier = isEvil . view components, buildRepr = \_ _ -> EvilTile }
    -- move enemy
  , def
    { qualifier = (\cs -> canMove cs && isEvil cs) . view components
    , everyTick = \_ e g ->
      (over
          entities
          (case
              gridAStar
                (g ^. entities)
                (e ^. posX, e ^. posY)
                ((\e' -> (e' ^. posX, e' ^. posY))
                  (head . filter (isPlayer . view components) $ g ^. entities)
                )
            of
              Just ((x, y) : _) -> flip (attemptMove x y) e
              _                 -> id
          )
        )
        g
    }
    -- render potion
  , def { qualifier = isPotion . view components
        , buildRepr = \_ _ -> PotionTile
        }
    -- drink potion
    -- TODO better way to formulate this?
  , def
    { qualifier = isPlayer . view components
    , everyTick = \c e g -> if c == Drink
                    then
                      case
                        filter
                          (\e' ->
                            e
                              ^. posX
                              == e'
                              ^. posX
                              && e
                              ^. posY
                              == e'
                              ^. posY
                              && (isPotion . view components) e'
                          )
                          (g ^. entities)
                      of
                        []    -> g -- drinking nothing doesn't do very much
                        p : _ -> over entities (delete p) g -- TODO apply effect
                    else g
    }
  ]
 where
   -- This has to prevent entities from walking on certain things
  attemptMove x y es e =
    let newEntity = (set posY y . set posX x) e
    in  case hasHealthAtLoc of
          e' : _ -> replace e' (setHealth (getHealth e' - 1) e') es
          []     -> if any (\e' -> isWall (e' ^. components) && sameLoc e') es
            then es
            else replace e newEntity es
   where
    hasHealthAtLoc =
      filter (\e' -> hasHealth (e' ^. components) && sameLoc e') es
    sameLoc e' = (e' ^. posX, e ^. posY) == (x, y)
  fromMoveCommand c es e =
    (case c of
        North -> attemptMove (e ^. posX) (e ^. posY - 1)
        East  -> attemptMove (e ^. posX + 1) (e ^. posY)
        South -> attemptMove (e ^. posX) (e ^. posY + 1)
        West  -> attemptMove (e ^. posX - 1) (e ^. posY)
        _     -> const
      )
      es
      e

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
getGrid =
  flip compose mkEntityGrid
    . map setEntity
    . sortOn (Down . view components)
    . (^. entities)
  where setEntity e = mset (e ^. posX) (e ^. posY) (Just e)


executeStep :: GameState -> GameState
executeStep = (compose . map runSystemET) systems . set message []

step :: (EntityGrid -> a) -> State GameState a
step render = do
  modify executeStep
  get <&> render . getGrid
