
module AutoPlayer (
    getHiddenCorner,
    findHiddenEdge,
    findHiddenTile,
    findSafeTile,
    findMine,
) where

import Data.List (sortBy)
import Constants (
    RowNum, ColNum, Grid(..), Tile(..), Status(..),
    Value(..), cols, rows)
import Mines (isVisible, isHidden, findTile,
    isFlagged, isInCol, isInRow, validGridList)


-- gets a hidden corner tile if there is one 
-- searches corners top, bottom, left, right in this order
getHiddenCorner :: Grid -> Maybe Tile
getHiddenCorner (Grid [] _) = Nothing
getHiddenCorner grid =
  case findTile (tiles grid) (0, 0) of -- top left
    Tile (r, c) v Hidden -> Just (Tile (r, c) v Hidden)
    _ -> case findTile (tiles grid) (0, cols (difficulty grid) - 1) of -- top right
      Tile (r, c) v Hidden -> Just (Tile (r, c) v Hidden)
      _ -> case findTile (tiles grid) (rows (difficulty grid)-1, 0) of -- bot left
        Tile (r, c) v Hidden -> Just (Tile (r, c) v Hidden)
        _ -> case findTile (tiles grid) (rows (difficulty grid)-1, cols (difficulty grid)-1) of -- bot right
          Tile (r, c) v Hidden -> Just (Tile (r, c) v Hidden)
          _           -> Nothing -- all corners opened/flagged

-- searches through the grid and returns a hidden tile that is definitely a mine
-- by checking visible tiles and their surrounding hidden tiles
findMine :: Grid -> Maybe Tile
findMine (Grid [] _)  = Nothing
findMine grid =
    let visibleTiles = getAllVisibleTiles (tiles grid) -- get all uncovered numbered tiles
        in case findMines grid visibleTiles of -- search for definite mines among hidden tiles
            []        -> Nothing
            (tile: _) -> Just tile

-- analyzes visible tiles to find hidden mines by using the following logic:
-- if a numbered tile has n remaining unflagged adjacent tiles
-- and needs n more mines, then all those tiles must be mines
findMines :: Grid -> [Tile] -> [Tile]
findMines (Grid [] _)  _ = []
findMines _ [] = []
findMines grid (tile: otherTiles) =
    case getNumFlaggedAndTiles grid tile of
        (0, _, _) -> findMines grid otherTiles
        (num, flaggedCount, hiddenTiles)
                | num - flaggedCount == length hiddenTiles && not (null hiddenTiles) -> hiddenTiles
                | otherwise -> findMines grid otherTiles


-- returns a safe (hidden) tile to open up
findSafeTile :: Grid -> Maybe Tile
findSafeTile (Grid [] _) = Nothing
findSafeTile grid =
  let visibleTiles = getAllVisibleTiles (tiles grid)
    in case findSafeTiles grid visibleTiles of
      []        -> Nothing
      (tile: _) -> Just tile

-- given a list of visible tiles, returns a list of safe hidden tiles to open
-- if a visible tile has number n and n mines are already flagged around it,
-- then the remaining hidden tiles around it are safe to open
findSafeTiles :: Grid -> [Tile] -> [Tile]
findSafeTiles (Grid [] _) _ = []
findSafeTiles _ [] = []
findSafeTiles grid (tile:otherTiles) = 
    case getNumFlaggedAndTiles grid tile of
        (0, _, _) -> findSafeTiles grid otherTiles
        (num, flaggedCount, hiddenTiles) 
            | num == flaggedCount && not (null hiddenTiles) -> hiddenTiles
            | otherwise -> findSafeTiles grid otherTiles

-- get neighbouring tiles of the tile provided, from the grid
getNeighbourTiles :: Grid -> Tile -> [Tile]
getNeighbourTiles grid tile =
  let validTuples = validGridList tile (rows (difficulty grid), cols (difficulty grid))
    in map (findTile (tiles grid)) validTuples

-- gets the tile number value, length of flagged tiles and hiddenTiles around it
getNumFlaggedAndTiles :: Grid -> Tile -> (Int, Int, [Tile])
getNumFlaggedAndTiles grid (Tile (r, c) (Num i) s) =
  let neighbours   = getNeighbourTiles grid (Tile (r, c) (Num i) s)
      flaggedTiles = filter isFlagged neighbours
      hiddenTiles  = filter isHidden neighbours
    in (i, length flaggedTiles, hiddenTiles)
getNumFlaggedAndTiles grid tile = --in case of a mine tile
  let neighbours   = getNeighbourTiles grid tile
      flaggedTiles = filter isFlagged neighbours
      hiddenTiles  = filter isHidden neighbours
    in (-1, length flaggedTiles, hiddenTiles)

-- Returns an arbitrary hidden tile located on the edges of the grid.
-- The function searches through all edges (top, bottom, left, right in this order)
-- and returns the first hidden tile found.
-- Returns Nothing if no hidden tiles exist on any edge
findHiddenEdge :: Grid -> Maybe Tile
findHiddenEdge (Grid [] _) = Nothing
findHiddenEdge grid =
  let gridTiles = tiles grid
      topEdge = filter (isInRow 0) gridTiles
      botEdge = filter (isInRow (rows (difficulty grid) - 1)) gridTiles
      leftEdge = filter (isInCol 0) gridTiles
      rightEdge = filter (isInCol (cols (difficulty grid) - 1)) gridTiles
      hiddenEdges = filter isHidden (topEdge ++ botEdge ++ leftEdge ++ rightEdge)
    in case hiddenEdges of
      []        -> Nothing
      (tile:_)  -> Just tile

-- returns a hidden tile, prioritizing those with fewer adjacent mines
findHiddenTile :: Grid -> Maybe Tile
findHiddenTile (Grid [] _) = Nothing
findHiddenTile grid =
    let allHiddenTiles = filter isHidden (tiles grid)
        tilesWithMineCount = map (\t -> (t, countAdjacentMines grid t)) allHiddenTiles
        sortedTiles = sortBy (\(_, c1) (_, c2) -> compare c1 c2) tilesWithMineCount
    in case sortedTiles of
        []          -> Nothing
        ((tile,_):_) -> Just tile

-- helper function to count adjacent mines
countAdjacentMines :: Grid -> Tile -> Int
countAdjacentMines grid tile =
    let neighbours = getNeighbourTiles grid tile
        visibleNeighbours = filter isVisible neighbours
    in sum [getNumberValue t | t <- visibleNeighbours]

-- helper function to get the numeric value of a tile
getNumberValue :: Tile -> Int
getNumberValue (Tile _ (Num n) _) = n
getNumberValue _ = 0

-- AI Logic helpers --
-- returns all tiles from a list of tiles, that are visible
getAllVisibleTiles :: [Tile] -> [Tile]
getAllVisibleTiles = filter isVisible

