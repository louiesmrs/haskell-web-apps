{-# LANGUAGE OverloadedStrings #-}
module Mines (
    chooseMines,
    createGrid,
    uncoverTile,
    flagOrUnflag,
    findTile,
    isVisible,
    isHidden,
    isFlagged,
    isInCol,
    isInRow,
    validGridList,
    updateGameStatus,
    forceUncoverTile
) where

import System.Random
import Constants (
    RowNum, ColNum, Grid(..), GameStatus(..), Tile(..), 
    Value(..), Status(..), cols, rows)



-- Generates a list of unique random mine locations within the grid dimensions
-- Takes a random number generator, grid dimensions (rows, cols), existing mine list,
-- and count of mines to place, returns tuple of (mine locations, updated generator)
chooseMines :: StdGen -> Int -> Int -> [(Int, Int)] -> Int -> ( [(Int, Int)], StdGen)
chooseMines g rows cols _ = generateUniqueCoordinates g (0, rows-1) (0, cols-1) []


-- Generates a list of unique random tuples within given bounds
-- Each tuple represents (row, col) coordinates for mine placement
generateUniqueCoordinates :: StdGen -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> ([(Int, Int)], StdGen)
generateUniqueCoordinates g _ _ currLst 0 = (currLst, g)
generateUniqueCoordinates g (minRow, maxRow) (minCol, maxCol) currLst count = 
    let (row, g1) = randomR (minRow, maxRow) g
        (col, g2) = randomR (minCol, maxCol) g1
        coord = (row, col)
    in if coord `elem` currLst
       then generateUniqueCoordinates g2 (minRow, maxRow) (minCol, maxCol) currLst count
       else generateUniqueCoordinates g2 (minRow, maxRow) (minCol, maxCol) (coord:currLst) (count-1)


-- Create complete grid with mines and numbers in single pass
-- Takes dimensions and mine positions
-- Returns complete grid with mines placed and numbers calculated
createGrid :: RowNum -> ColNum -> [(RowNum, ColNum)] -> [Tile]
createGrid rows cols mines = concat [createRow r rows cols mines | r <- [0..rows-1]]

-- Create row with mines and calculated adjacent mine counts
-- Places mines and calculates numbers for each tile
-- Uses validGridList to check surrounding positions
-- Updates tile values based on adjacent mine count
createRow :: RowNum -> RowNum -> ColNum -> [(RowNum, ColNum)] -> [Tile]
createRow row rows cols mines = 
    [createTile row col mines | col <- [0..cols-1]]
    where
        createTile r c mineList
            | (r, c) `elem` mineList = Tile (r, c) Mine Hidden
            | otherwise = Tile (r, c) (Num adjacentCount) Hidden
            where 
                adjacentCount = length $ filter (`elem` mineList) $ 
                    validGridList (Tile (r, c) (Num 0) Hidden) (rows, cols)


-- Simple Boolean Funcs for Tiles (can be used for filter easily) --
-- True if tile has a mine, False otherwise
isMine :: Tile -> Bool
isMine (Tile (_, _) Mine _)    = True
isMine _                       = False

-- True if tile is hidden, false otherwise
isHidden :: Tile -> Bool
isHidden (Tile (_, _) _ Hidden)    = True
isHidden _                         = False

-- True if tile is Visible, false otherwise
isVisible :: Tile -> Bool
isVisible (Tile (_, _) _ Visible)     = True
isVisible _                         = False

-- True if tile is Flagged, false otherwise
isFlagged :: Tile -> Bool
isFlagged (Tile (_, _) _ Flagged)     = True
isFlagged _                           = False

-- True is tile is in the row specified
isInRow :: RowNum -> Tile -> Bool
isInRow row (Tile (r, _) _ _) = row == r

-- True is tile is in the column specified
isInCol :: ColNum -> Tile -> Bool
isInCol col (Tile (_, c) _ _) = col == c


-- Selectively apply a transformation function to tiles that are neighbors of a given center tile,
-- while leaving non-neighboring tiles unchanged. 
gridMap :: [Tile] -> (Tile -> Tile) -> Tile -> [Tile]
gridMap [] _ _ = []
gridMap ((Tile (row, col) val status) :grid) func tile =
    let tileNums = gridList tile
        in (if isMember (row, col) tileNums
                then func (Tile (row, col) val status) : gridMap grid func tile
                else Tile (row, col) val status : gridMap grid func tile)

-- returns a list of locations of neighbouring tiles, excluding centre tile's location
gridList :: Tile -> [(RowNum, ColNum)]
gridList (Tile (row, col) _ _) =
    [(row-1, col-1), (row-1, col), (row-1, col+1),
     (row, col-1), {- (row, col), -} (row, col+1),
     (row+1, col-1), (row+1, col), (row+1, col+1)]

-- checks if a tile with (RowNum, ColNum) must exist in the grid  
inRange :: (RowNum, ColNum) -> (Int, Int) -> Bool
inRange (r, c) (rows, cols)
    | r >= 0 && c >= 0 && r < rows && c < cols = True
    | otherwise                                = False

-- returns a list of locations of neighbouring-tiles (excluding centre tile's location) IF THEY ARE VALID LOCATIONS
validGridList :: Tile -> (Int, Int) -> [(RowNum, ColNum)]
validGridList tile (rows, cols) = filter (\x -> inRange x (rows, cols)) (gridList tile)

-- modify grid to uncover/uncover a tile if you can (if it's hidden)
uncoverTile :: Grid -> Tile -> ([Tile], [Tile])
uncoverTile grid (Tile (row, col) val Hidden)
    | val == Num 0 = clear grid (Tile (row, col) val Hidden)  -- Use clear for tiles with value 0
    | otherwise =
        let uncovered = uncoverSingleTile $ Tile (row, col) val Hidden
            newGrid = replace (tiles grid) (Tile (row, col) val Hidden) uncovered
        in (newGrid, [uncovered])
uncoverTile (Grid tiles _) _ = (tiles, [])

-- uncover/uncover a tile if you can (if it's hidden)
uncoverSingleTile :: Tile -> Tile
uncoverSingleTile (Tile (row, col) val Hidden) = Tile (row, col) val Visible
uncoverSingleTile tile = tile

-- Recursively clear tiles starting from a tile with value 0
clear :: Grid -> Tile -> ([Tile], [Tile])
clear grid tile@(Tile (row, col) val Hidden) =
  let uncovered = uncoverSingleTile tile
      newTiles = replace (tiles grid) tile uncovered
      neighbors = validGridList tile (rows (difficulty grid), cols (difficulty grid))
      neighborTiles = map (findTile newTiles) neighbors
      hiddenNeighbors = filter isHidden neighborTiles
  in if val == Num 0
     then foldr (\c (acc, changed) ->
            let (newAcc, newChanged) = uncoverTile (Grid acc (difficulty grid)) c
            in (newAcc, newChanged ++ changed))
          (newTiles, [uncovered]) hiddenNeighbors
     else (newTiles, [uncovered])
clear (Grid tiles _) _ = (tiles, [])


-- modify grid to uncover/uncover a tile regardless of it's status
forceUncoverTile :: [Tile] -> Tile -> [Tile]
forceUncoverTile grid (Tile (row, col) val stat) =
  replace grid (Tile (row, col) val stat) (Tile (row, col) val Visible)

-- flag a tile if it's Hidden, unflag if if it's flagged
flagOrUnflag :: [Tile] -> Tile -> [Tile]
flagOrUnflag grid (Tile (row, col) val Hidden) =
            replace grid (Tile (row, col) val Hidden) (Tile (row, col) val Flagged)
flagOrUnflag grid (Tile (row, col) val Flagged) =
            replace grid (Tile (row, col) val Flagged) (Tile (row, col) val Hidden)
flagOrUnflag grid _ = grid


-- increment the Num value of a tile
incrTileVal :: Tile -> Tile
incrTileVal (Tile (r, c) (Num i) status) = Tile (r, c) (Num (i+1)) status
incrTileVal tile = tile -- for mine-tile cases

-- checks if in alement exists in a list
isMember :: (Eq a) => a -> [a] -> Bool
isMember _ []        = False
isMember elem (x:xs) = (elem == x) || isMember elem xs

-- checks if the game ended (grid = latest updated grid) (tile = tile chosen)
updateGameStatus :: [Tile] -> Tile -> GameStatus
updateGameStatus grid (Tile (row, col) val stat)
    | stat == Flagged                   = Running
    | isMine (Tile (row, col) val stat) = Loss
    | correctFlagging grid || onlyMinesHidden grid = Win
    | otherwise                                    = Running

-- checks if only mine tiles are still hidden
onlyMinesHidden :: [Tile] -> Bool
onlyMinesHidden grid =
    let hiddenTiles = filter isHidden grid
        nonMineTiles = filter (not . isMine) grid
    in all isMine hiddenTiles && all isVisible nonMineTiles && not (null hiddenTiles)

-- checks if number of flags equals number of mines and they're correctly placed
correctFlagging :: [Tile] -> Bool
correctFlagging grid =
    let flaggedTiles = filter isFlagged grid
        mineTiles = filter isMine grid
        nonMineTiles = filter (not . isMine) grid
    in length flaggedTiles == length mineTiles && 
       all (\t -> any (\m -> tilePosition t == tilePosition m) mineTiles) flaggedTiles &&
       all isVisible nonMineTiles
    where tilePosition (Tile pos _ _) = pos

-- Replaces all occurrences of an element with a new element in a list
-- Example: replace [1,2,1,3] 1 4 = [4,2,4,3]
replace :: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) old new
    | x == old  = new : replace xs old new
    | otherwise = x   : replace xs old new
    
-- Locates and returns a specific tile from the game grid based on its row and column coordinates.
-- Traverses through the list of tiles until it finds a matching position
findTile :: [Tile] -> (RowNum, ColNum) -> Tile
findTile [] (row, col) = error $ "findTile: tile not found in grid " ++ show row ++ ", " ++  show col
findTile ((Tile (r, c) val stat): grid) (row, col) =
  if r == row && c == col
    then Tile (r, c) val stat
    else findTile grid (row, col)

