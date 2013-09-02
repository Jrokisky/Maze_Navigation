import System.IO
import System.Cmd
import Data.Char
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import System.Random (randomRIO)

height = 30
width =  100

type Grid = UArray (Int, Int) Char
type Location = (Int, Int)
data Cell = Cell { loc :: Location
                 , value :: Char
                 } deriving (Eq)

main = do
    hSetBuffering stdin NoBuffering
    let grid = editGrid (1,1) '@' $ buildGrid height width
    maze <- buildMaze grid $ getWalls (1,1) 
    gridLoop maze (1,1)

-- | Draw Maze. Process player movement.
gridLoop :: Grid
         -> Location
         -> IO()
gridLoop maze location = do
    system "clear"
    putStrLn $ showGrid maze 
    move <- getChar
    let newLocation = updateLocation height width move location maze
    let updatedMaze = editGrid location  '.' maze
    let finalMaze = editGrid newLocation '@' updatedMaze 
    gridLoop finalMaze newLocation 


-- | Build a Grid
buildGrid :: Int    --Height
          -> Int    --Width 
          -> Grid   --Grid 
buildGrid h w = runSTUArray $ do 
    let symbols = [c | y <- [0..h], x <- [0..w], let c = getSymbol (y,x)]
    grid <- newListArray ((0,0), (h,w)) symbols
    return grid

-- | Build a maze from a grid
buildMaze :: Grid 
          -> [Cell]  --list of walls
          -> IO Grid
buildMaze grid walls = 
    case walls of
      [wall]   -> do
                  let (maze, _) = analyzeWall grid wall
                  return maze
      walls    -> do 
                  (wall, newWalls) <- pick walls
                  let (maze, additionalWalls) = analyzeWall grid wall
                  buildMaze maze $ additionalWalls ++ newWalls

-- | Pick random value from list. Return list sans value.
pick :: [a] -> IO (a, [a])
pick [x] = return (x, [])
pick xs = do 
      idx <- randomRIO (1, length xs -1)
      let (front, back) = splitAt idx xs
      return (head . reverse $ front, (init front) ++ back)

-- | analyze Wall. Edit maze
analyzeWall :: Grid
            -> Cell
            -> (Grid, [Cell])
analyzeWall grid w =
    case unvisitedNeighbor w grid of
      []   -> (grid, [])
      [uv] -> let grid2 = editGrid (loc w) ' ' grid
                  grid3 = editGrid (loc uv) ' ' grid2
              in (grid3, getWalls $ loc uv)

-- | get adjacent unvisited cell of a wall
-- | (this is hack code. Should only ever return one neighbor) 
unvisitedNeighbor :: Cell    --wall
                 -> Grid
                 -> [Cell]
unvisitedNeighbor (Cell (y,x) c) grid
    | isBorder (y,x) = []
    | c == '|' = filter (unvisited) $ (getCell grid (y, (x-1))) : (getCell grid (y, (x+1))) : [] 
    | c == '=' = filter (unvisited) $ (getCell grid ((y-1), x)) : (getCell grid ((y+1), x)) : []
    | otherwise = []
  where unvisited x = '#' == value x
        getCell g loc = Cell loc (grid ! loc)
              

-- | Get non-border walls of cell
getWalls :: Location   
         -> [Cell] 
getWalls (y,x) = 
    let up    = if isBorder ((y-1),x) then [] else [(Cell ((y-1),x) '=')]
        down  = if isBorder ((y+1),x) then [] else [(Cell ((y+1),x) '=')]
        left  = if isBorder (y,(x-1)) then [] else [(Cell (y,(x-1)) '|')]
        right = if isBorder (y,(x+1)) then [] else [(Cell (y,(x+1)) '|')]
    in up ++ down ++ left ++ right


-- | True if location is border
isBorder :: Location  --to evaluate
         -> Bool
isBorder (y,x) = y == 0 || y == height || x == 0 || x == width


-- | Edit the given Grid
editGrid :: Location
         -> Char
         -> Grid       --Old Grid
         -> Grid       --New Grid
editGrid (y,x) c grid = runSTUArray $ do
    stGrid <- thaw grid
    writeArray stGrid (y,x) c
    return stGrid

showGrid :: Grid -> [Char]
showGrid arr = insertEvery 1 w "\n" $ elems arr
    where w = 1 + (snd . snd . bounds $ arr)


-- | Update Location of player
updateLocation :: Int      --height 
               -> Int      --width
               -> Char     --move
               -> Location --current location
               -> Grid     --current grid
               -> Location
updateLocation h w m (y, x) g
    | m == 'w' && y /= 0  = if isMovable (y-1) x then ((y-1), x) else (y,x)
    | m == 's' && y /= h  = if isMovable (y+1) x then ((y+1), x) else (y,x)
    | m == 'a' && x /= 0  = if isMovable y (x-1) then (y, (x-1)) else (y,x) 
    | m == 'd' && x /= w  = if isMovable y (x+1) then (y, (x+1)) else (y,x)
    | otherwise = (y,x)
  where isMovable uY uX = any (== (g ! (uY, uX))) " ." 

-- | Insert string after every 'div' characters
insertEvery :: Int -> Int -> [Char] -> [Char] -> [Char]
insertEvery _ _ _ [x]     = [x]
insertEvery cnt div ins (x:xs) 
    | cnt == div = x : ins ++ (insertEvery 1 div ins xs)
    | otherwise  = x : (insertEvery (cnt + 1) div ins xs)

-- | Get the symbol for a cell
getSymbol :: Location -> Char
getSymbol (y,x) 
    | even y && even x = '+'
    | even y && odd x  = '='
    | odd y && even x  = '|'
    | odd y && odd x   = '#'
