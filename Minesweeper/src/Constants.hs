{-# LANGUAGE OverloadedStrings #-}

module Constants(
    canvasHeight, canvasWidth, rows, cols,
    mines, defaultDifficulty,
    RowNum, ColNum, Grid(..), GameStatus(..), Tile(..), ClickMode(..), 
    Value(..), Status(..), Difficulty(..), tileWidth, tileHeight, xGap, yGap
) where 


-- Modelling the Tile --
data Value = Mine | Num Int -- Mine or not
                deriving (Eq, Show)
data Status = Hidden | Visible | Flagged -- status of tile
                deriving (Eq, Show)
type RowNum = Int -- from 0 to rows-1
type ColNum = Int -- from 0 to cols-1

-- Tile definition
data Tile = Tile (RowNum, ColNum) Value Status
              deriving (Eq, Show)


-- A grid is represented by 1D list (we have position in grid as part of tile) and a difficulty
-- Modelling the grid --
data Grid = Grid {
  tiles :: [Tile],
  difficulty :: Difficulty
} deriving (Show)


-- status of the game
data GameStatus = Loss | Win | Running | Start
              deriving (Eq, Show)

-- left click right click replacement
data ClickMode = UncoverMode | FlagMode
              deriving (Eq)


-- difficulty levels
data Difficulty = Easy | Medium | Hard
              deriving (Eq, Show)

-- default difficulty
defaultDifficulty :: Difficulty
defaultDifficulty = Easy

-- number of rows and columns of tiles in the grid based on difficulty
rows :: Difficulty -> RowNum
rows Easy = 9
rows Medium = 16
rows Hard = 16

cols :: Difficulty -> ColNum
cols Easy = 9
cols Medium = 16
cols Hard = 30

-- number of mines in the grid based on difficulty
mines :: Difficulty -> Int
mines Easy = 10
mines Medium = 40
mines Hard = 99

-- width and height of tile in minesweeper
tileWidth = 28.0
tileHeight = tileWidth -- keep it as a square

-- spacing between tiles
xGap = 3.0
yGap = 2.0

-- width and height of canvas based on difficulty
canvasWidth :: Difficulty -> Double
canvasWidth difficulty = colsNum * tileWidth + (colsNum + 1) * xGap
                where colsNum = fromIntegral (cols difficulty)

canvasHeight :: Difficulty -> Double
canvasHeight difficulty = rowsNum * tileHeight + (rowsNum + 1) * yGap 
                where rowsNum = fromIntegral (rows difficulty)