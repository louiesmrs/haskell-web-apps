{-# Language ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import System.Random
import Data.IORef
import Control.Monad.Trans (liftIO)
import Control.Monad (void, forM_)
import Constants (canvasHeight, canvasWidth, rows, cols,
    mines, Difficulty (Easy, Medium, Hard), defaultDifficulty,
    RowNum, ColNum, Grid(..), GameStatus(..), Tile(..), ClickMode(..),
    Value(..), Status(..), tileWidth, tileHeight, xGap, yGap)
import Mines(chooseMines,
    createGrid,
    uncoverTile,
    flagOrUnflag,
    findTile,
    forceUncoverTile,
    updateGameStatus
    )
import AutoPlayer(
    findSafeTile,
    findMine,
    getHiddenCorner,
    findHiddenTile,
    findHiddenEdge)




main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set title "Minesweeper"

  screen <- UI.canvas
    # set UI.width (ceiling (canvasWidth defaultDifficulty))  -- use current mode for dimensions
    # set UI.height (ceiling (canvasHeight defaultDifficulty))
    # set UI.style [("border", "solid black 1px"), ("background", "#ddd")]
    # set UI.textAlign UI.Center

  let baseButtonStyle = [("margin", "5px"),
                    ("padding", "5px 10px"),
                    ("font-size", "12px"),
                    ("border-radius", "5px"),
                    ("border", "none"),
                    ("cursor", "pointer"),
                    ("color", "white")]

  easy <- UI.button
    # set UI.style (("background-color", "#4CAF50") : baseButtonStyle)
    #+ [string "Easy"]
  medium <- UI.button
    # set UI.style (("background-color", "#2196F3") : baseButtonStyle)
    #+ [string "Medium"]
  hard <- UI.button
    # set UI.style (("background-color", "#FF9800") : baseButtonStyle)
    #+ [string "Hard"]
  reset <- UI.button
    # set UI.style (("background-color", "#f44336") : baseButtonStyle)
    #+ [string "Reset game"]
  changeMode <- UI.button
    # set UI.style (("background-color", "#4CAF50") : baseButtonStyle)
    #+ [string "Flag Mode"]
  playButton <- UI.button
    # set UI.style (("background-color", "#607D8B") : baseButtonStyle)
    #+ [string "Play move"]

  mineCount <- UI.span
    # set UI.style [("font-family", "Courier, monospace"),
                    ("font-size", "24px"),
                    ("display", "block"),
                    ("margin", "5px")]
    #+ [string $ "Mines: " ++ show (mines defaultDifficulty)]

    -- IOReferences (Variables)
  coord <- liftIO $ newIORef (0,0) -- coordinates of mouse on screen
  g     <- liftIO newStdGen -- uses the split method to create newGen
  grid <- liftIO $ newIORef (Grid {
    tiles = [],
    difficulty = defaultDifficulty
  })
  clickMode <- liftIO $ newIORef UncoverMode -- you uncover mines by default
  gameStat <- liftIO $ newIORef Start -- current game status


  -- add screen, and play as human/AI buttons
  getBody window #+
    [row [element screen], element easy, element medium, element hard]

  -- Then hook up the click handlers:
  on UI.click easy $ \_ -> handleDifficultyClick easy Easy window screen easy medium hard reset changeMode playButton mineCount g grid gameStat
  on UI.click medium $ \_ -> handleDifficultyClick medium Medium window screen easy medium hard reset changeMode playButton mineCount g grid gameStat
  on UI.click hard $ \_ -> handleDifficultyClick hard Hard window screen easy medium hard reset changeMode playButton mineCount g grid gameStat

  on UI.click changeMode $ \_ ->
    do
      mode <- liftIO $ readIORef clickMode -- get current mode
      case mode of
        UncoverMode -> -- if it's in uncovermode, change to flag mode
          do
            liftIO $ writeIORef clickMode FlagMode
            element changeMode  -- change button text appropriately
              # set UI.text "Uncover Mode"
        FlagMode -> -- if it's in flagmode, change to uncover mode
          do
            liftIO $ writeIORef clickMode UncoverMode
            element changeMode  -- change button text appropriately
              # set UI.text "Flag Mode"

  -- play move button for the AI
  on UI.click playButton $ \_ ->
    do
      gridVal    <- liftIO $ readIORef grid -- read grid
      gameStatVal <- liftIO $ readIORef gameStat -- read game status
      (newGrid, newGameStatus) <- playMove gridVal gameStatVal screen
      liftIO $ writeIORef grid newGrid -- write new grid
      liftIO $ writeIORef gameStat newGameStatus -- write new game status
  on UI.mousemove screen $ \(x,y) ->
    do liftIO $ writeIORef coord (x,y) -- save the current mouse coordinates

  -- respond to clicks onscreen only if Human is playing an unfinished game
  on UI.click screen $ \_ ->
    do
      currGameStatus <- liftIO $ readIORef gameStat -- read the game's status
      case currGameStatus of -- only proceed to play if game is ongoing and the player is playing
        Running ->
          do
            (x, y)     <- liftIO $ readIORef coord
            mode    <- liftIO $ readIORef clickMode
            case mode of
              UncoverMode -> -- uncovering the tile
                do
                  gridVal    <- liftIO $ readIORef grid -- read grid
                  gameStatVal <- liftIO $ readIORef gameStat -- read game status
                  (newGrid, newGameStatus) <- uncoverResponse gridVal gameStatVal screen (fromIntegral (ceiling x), fromIntegral (ceiling y)) --uncover func
                  liftIO $ writeIORef grid newGrid -- write newGrid
                  liftIO $ writeIORef gameStat newGameStatus -- write new game Status
              FlagMode   -> -- un/flagging the tile
                do
                  gridVal    <- liftIO $ readIORef grid -- read grid
                  newGrid <- flagResponse gridVal screen (fromIntegral (ceiling x), fromIntegral (ceiling y)) -- flag func
                  liftIO $ writeIORef grid newGrid -- write newGrid
        _    -> return () -- case game over

  -- button to reset the game
  on UI.click reset $ \_ -> -- reset = delete everything, start over
    do
      UI.delete screen -- delete everything
      UI.delete reset
      UI.delete changeMode
      UI.delete playButton
      UI.delete mineCount
      setup window -- start over from the beginning


handleDifficultyClick :: Element -> Difficulty -> Window -> UI.Canvas -> Element -> Element -> Element -> Element -> Element -> Element -> Element -> StdGen -> IORef Grid -> IORef GameStatus -> UI ()
handleDifficultyClick button difficulty window screen easy medium hard reset changeMode playButton mineCount g grid gameStat =
  do
    UI.delete easy
    UI.delete medium
    UI.delete hard
    screen # set' UI.width (ceiling (canvasWidth difficulty))
    screen # set' UI.height (ceiling (canvasHeight difficulty))
    let newGrid = Grid {
      tiles = createGrid (rows difficulty) (cols difficulty) (fst $ chooseMines g (rows difficulty) (cols difficulty) [] (mines difficulty)),               
      difficulty = difficulty
    }
    liftIO $ writeIORef grid newGrid
    liftIO $ writeIORef gameStat Running
    screen # set' UI.fillStyle (UI.htmlColor "#999")
    createGridUI (0.0, 0.0) (rows difficulty) (cols difficulty) screen
    element mineCount # set text ("Mines: " ++ show (mines difficulty))
    void $ getBody window #+ [element reset, element changeMode, element playButton, element mineCount]

createGridUI :: UI.Point -> RowNum -> ColNum -> UI.Canvas -> UI ()
createGridUI (xPos, yPos) rows cols screen | rows < 1 =
                                                error "createGridUI: rows < 1"
createGridUI (xPos, yPos) 1 cols screen =
    createRowUI (xPos + xGap, yPos + yGap) cols screen --create last row
createGridUI (xPos, yPos) rows cols screen =
  do
    createRowUI (xPos + xGap, yPos + yGap) cols screen
    createGridUI (xPos, yPos + yGap + tileHeight) (rows - 1) cols screen

-- creates the UI for a row of tiles
createRowUI :: UI.Point -> ColNum -> UI.Canvas -> UI ()
createRowUI coord cols screen | cols < 1 = error "createRowUI: cols < 1"
createRowUI coord 1 screen = screen # UI.fillRect coord tileWidth tileHeight --last tile creation (in last column)
createRowUI (xPos, yPos) cols screen =
  do
   screen # UI.fillRect (xPos, yPos) tileWidth tileHeight -- tiles just rects
   createRowUI (xPos + tileWidth + xGap, yPos) (cols - 1) screen

-- responds to click on the screen by the Human, when in UncoverMode
uncoverResponse :: Grid -> GameStatus -> UI.Canvas -> UI.Point -> UI (Grid, GameStatus)
uncoverResponse grid gameStat screen coord =
    let (rowIndex, colIndex) = getClickedTileNum coord
        tileClicked          = findTile (tiles grid) (rowIndex, colIndex)
      in makeMove screen grid tileClicked

-- responds to click on the screen by the Human, when in FlagMode
flagResponse :: Grid -> UI.Canvas -> UI.Point -> UI Grid
flagResponse grid screen coord =
    let (rowIndex, colIndex) = getClickedTileNum coord
        tileClicked          = findTile (tiles grid) (rowIndex, colIndex)
      in do
          manageFlagging screen grid tileClicked

-- make move for Auto Solver (called when play move button is pressed)
playMove :: Grid -> GameStatus -> UI.Canvas -> UI (Grid, GameStatus)
playMove grid gameStat canvas =
  case findSafeTile grid of -- first try to find guaranteed safe tiles
   Just tile -> makeMove canvas grid tile
   Nothing   ->
     case findMine grid of -- if no safe moves, flag any obvious mines
       Just tile ->
        do
          newGrid <- manageFlagging canvas grid tile
          return (newGrid, gameStat)
       Nothing   ->
            case getHiddenCorner grid of -- if stuck, try corners (often safer)
              Just tile -> makeMove canvas grid tile
              Nothing   ->
                case findHiddenEdge grid of -- then edges
                  Just tile -> makeMove canvas grid tile
                  Nothing   ->
                    case findHiddenTile grid of -- last resort
                      Just tile -> makeMove canvas grid tile
                      Nothing   -> return (grid, gameStat)

-- handles attempt to uncover a tile both on board and screen
-- if 0 tile is uncovered, updates screen to make neighbouring 0 tiles and
-- adjacent tiles to any uncovered 0 tiles Visible
makeMove :: UI.Canvas -> Grid -> Tile -> UI (Grid, GameStatus)
makeMove screen grid (Tile (r, c) v s) =
  -- returns 2 arrays: newTiles: updated array of all tiles. changed: array of only updated tiles.
  let (newTiles, changed) =  uncoverTile grid (Tile (r, c) v s)
      newGrid =  Grid { tiles = newTiles, difficulty = difficulty grid }
    in do
        tileLocation <- getTileStartPt (r, c) -- get tile location
        -- If the uncovered tile was 0, changed will be array of neighbouring tiles
        forM_ changed $ \tile@(Tile (r', c') val _) -> do
            (xPos, yPos) <- getTileStartPt (r', c')
            screen # set' UI.fillStyle (UI.htmlColor "white")
            screen # UI.fillRect (xPos, yPos) tileWidth tileHeight
            void $ screen # set' UI.textFont "12px monospace"  -- Reset font
            void $ screen # set' UI.textAlign UI.Center
            case val of
              Num n -> do
                setNumberColor screen n
                screen # UI.strokeText (show n) (xPos + tileWidth/2, yPos + tileHeight/2)
              _ -> return ()
        (newestGrid, gameStatus) <- updateGridNStatus screen newGrid (Tile (r, c) v s) tileLocation -- update the grid and game status
        liftIO $ print $ show (Tile (r, c) v s) ++ " --- " ++ show newGrid
        return (newestGrid, gameStatus)

-- handles attempt to flag a tile
manageFlagging :: UI.Canvas -> Grid -> Tile -> UI Grid
manageFlagging screen grid (Tile (r, c) v s) =
  let newGrid = Grid { tiles = flagOrUnflag (tiles grid) (Tile (r, c) v s), difficulty = difficulty grid }
    in do
        (xPos, yPos) <- getTileStartPt (r, c)
        handleFlaggingUI screen (Tile (r, c) v s) (xPos, yPos)
        return newGrid

-- updates the grid UI based on flagging/unflagging a tile
handleFlaggingUI :: UI.Canvas -> Tile -> UI.Point -> UI ()
handleFlaggingUI screen (Tile (_, _) _ stat) (xPos, yPos) =
  case stat of
    Hidden  -> -- mark all flags with an F, if tile was hidden
      do
        void $ screen # set' UI.strokeStyle "black"
        screen # UI.strokeText "F" (xPos + tileWidth/2, yPos + tileHeight/2)
    Flagged -> -- if tile was flagged, unflag it (make it hidden)
      do
        void $ screen # set' UI.fillStyle (UI.htmlColor "#999")
        screen # UI.fillRect (xPos, yPos) tileWidth tileHeight
    _       -> -- cant flag or unflag a tile that's already uncovered
      return ()

-- gets the tile number that was clicked on by the Human player (maps click coordinates to tile pos)
getClickedTileNum :: UI.Point -> (RowNum, ColNum)
getClickedTileNum (x, y) =
  (floor $ y/(yGap + tileHeight), floor $ x/(xGap + tileWidth) )

-- get the tile's top-left coordinates, just a calculation function
getTileStartPt :: (RowNum, ColNum) -> UI UI.Point
getTileStartPt (r, c) =
  let rowNum               = fromIntegral r
      colNum               = fromIntegral c
      xPos                 = (colNum + 1) * xGap + colNum * tileWidth
      yPos                 = (rowNum + 1) * yGap + rowNum * tileHeight
    in return (xPos, yPos)

-- Updates the grid UI based on game outcome and tile clicked
updateGridNStatus :: UI.Canvas -> Grid -> Tile -> UI.Point -> UI (Grid, GameStatus)
updateGridNStatus screen grid tile tileLocation =
  case updateGameStatus (tiles grid) tile of
    Loss    -> do -- if it's a loss, show a loss and end the game
                newGrid <- endGame grid screen
                lossMessage screen tileLocation grid
                return (newGrid, Loss)
    Win     -> do -- if it's a win, show the win and end the game
                newGrid <- endGame grid screen
                winMessage screen tileLocation tile grid
                return (newGrid, Win)
    Running -> do -- just uncover the tile, and carry on
                uncoverOnScreen screen tileLocation tile
                return (grid, Running)

-- display mine that caused loss and loss message
lossMessage :: UI.Canvas -> UI.Point -> Grid -> UI ()
lossMessage screen (xPos,yPos) grid =
  do
   -- Show X with red backgroudn for the mine that caused loss
   screen # set' UI.fillStyle (UI.htmlColor "red")
   screen # UI.fillRect (xPos,yPos) tileWidth tileHeight
   void $ screen # set' UI.strokeStyle "black"
   screen # UI.strokeText "X" (xPos + tileWidth/2, yPos + tileHeight/2)
   let width = canvasWidth $ difficulty grid
       height = canvasHeight $ difficulty grid
       yOffset = max (height/4) (min (height/2) yPos)  -- Keep between 1/4 and 1/2 of height
       xOffset = width/2  -- Always center the text horizontally
   void $ screen # set' UI.textFont "48px Arial"
   void $ screen # set' UI.strokeStyle "#ff1300"
   void $ screen # set' UI.textAlign UI.Center
   screen # UI.strokeText "You lose" (xOffset, yOffset)


-- display final non-mine tile and win message
winMessage :: UI.Canvas -> UI.Point -> Tile -> Grid -> UI ()
winMessage screen tileLocation tile grid =
  do
    uncoverOnScreen screen tileLocation tile
    let width = canvasWidth $ difficulty grid
        height = canvasHeight $ difficulty grid
    void $ screen # set' UI.textFont "48px Arial"
    void $ screen # set' UI.strokeStyle "#008100"
    void $ screen # set' UI.textAlign UI.Center
    screen # UI.strokeText "You win" (width/2, height/2)



-- show UI stuff for opening of a hidden tile, if it's not a mine
uncoverOnScreen :: UI.Canvas -> UI.Point -> Tile -> UI ()
uncoverOnScreen screen (xPos, yPos) tile =
  case tile of
    Tile (_, _) (Num i) Hidden ->
      do
       screen # set' UI.fillStyle (UI.htmlColor "white")
       screen # UI.fillRect (xPos, yPos) tileWidth tileHeight
       void $ screen # set' UI.textFont "12px monospace"  -- Reset font
       void $ screen # set' UI.textAlign UI.Center
       setNumberColor screen i  -- Set color based on number
       screen # UI.strokeText (show i) (xPos + tileWidth/2, yPos + tileHeight/2)
    _                 -> return ()

-- puts an end to the game (by uncovering all other tiles)
endGame :: Grid -> UI.Canvas -> UI Grid
endGame grid = uncoverRemainingTiles grid grid -- pass grid twice as that function needs a grid copy as well

-- uncover all tiles which aren't Shown yet
uncoverRemainingTiles :: Grid -> Grid -> UI.Canvas -> UI Grid
uncoverRemainingTiles newGrid (Grid [] _) _ = return newGrid
uncoverRemainingTiles gridCopy (Grid (tile: otherTiles) difficulty) screen =
    case tile of
      Tile (_, _) _ Visible -> uncoverRemainingTiles gridCopy (Grid otherTiles difficulty) screen
      _           ->
        do
          newGrid <- forceUncoverTileComplete gridCopy tile screen
          uncoverRemainingTiles newGrid (Grid otherTiles difficulty) screen

-- uncovers a tile (UI and programatically) regardless of it's current status
forceUncoverTileComplete :: Grid -> Tile -> UI.Canvas -> UI Grid
forceUncoverTileComplete grid (Tile (rowNum, colNum) val stat) screen =
  let newGrid      = Grid { tiles = forceUncoverTile (tiles grid) (Tile (rowNum, colNum) val stat), difficulty = difficulty grid }
      col           = fromIntegral colNum
      row           = fromIntegral rowNum
      xPos          = (col + 1) * xGap + col * tileWidth
      yPos          = (row + 1) * yGap + row * tileHeight
      tileLocation  = (xPos, yPos)
    in do
      void $ screen # set' UI.textFont "12px monospace"  -- Reset font
      void $ screen # set' UI.textAlign UI.Center
      case val of
        Mine -> -- mines shown as X
          do
          screen # set' UI.fillStyle (UI.htmlColor "white")
          screen # UI.fillRect tileLocation tileWidth tileHeight
          void $ screen # set' UI.strokeStyle "black"
          screen # UI.strokeText "X" (xPos + tileWidth/2, yPos + tileHeight/2)
          return newGrid
        (Num i) -> -- Number tiles shown as number of neighbouring mines with appropriate color
          do
          screen # set' UI.fillStyle (UI.htmlColor "white")
          screen # UI.fillRect tileLocation tileWidth tileHeight
          setNumberColor screen i  -- Set color based on number
          screen # UI.strokeText (show i) (xPos + tileWidth/2, yPos + tileHeight/2)
          return newGrid

-- Helper function to set the color based on number
setNumberColor :: UI.Canvas -> Int -> UI ()
setNumberColor canvas n = do
  case n of
    1 -> void $ canvas # set' UI.strokeStyle "#0000ff"
    2 -> void $ canvas # set' UI.strokeStyle "#008100"
    3 -> void $ canvas # set' UI.strokeStyle "#ff1300"
    4 -> void $ canvas # set' UI.strokeStyle "#000083"
    5 -> void $ canvas # set' UI.strokeStyle "#810500"
    6 -> void $ canvas # set' UI.strokeStyle "#2a9494"
    7 -> void $ canvas # set' UI.strokeStyle "#000000"
    8 -> void $ canvas # set' UI.strokeStyle "#808080"
    _ -> void $ canvas # set' UI.strokeStyle "black"



