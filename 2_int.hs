import System.IO
import System.Cmd
import Data.Char

data Board = Board [[Char]] 
data Location = Location (Int, Int)

instance Show Board where
    show (Board x) = unlines x

height = 40
width = height * 3

main = do
    hSetBuffering stdin NoBuffering
    let originalLocation = Location (0,0)
    let originalBoard = createBoard height originalLocation 0 []
    boardLoop originalBoard originalLocation

--Main Loop where board is redrawn and player movement is processed
boardLoop :: Board -> Location -> IO()
boardLoop board location = do
    system "clear"
    putStr $ show board
    move <- getChar
    let newLocation = updateLocation move location
    let newBoard = createBoard height newLocation 0 []
    boardLoop newBoard newLocation 

--Creates a Board
createBoard :: Int -> Location -> Int -> [[Char]] ->  Board
createBoard height (Location (x,y)) row b 
    | row == y           = createBoard height location nxtRow $ playerRow : b
    | row == height + 1  = Board b
    | otherwise          = createBoard height location nxtRow $ normalRow : b
  where
     playerRow = map (\z -> if z == x then '@' else '.') [0..width]
     normalRow = replicate (width + 1) '.' 
     nxtRow = row + 1
     location = Location (x,y)

--Update Location
updateLocation :: Char -> Location -> Location
updateLocation m (Location (x,y)) 
    | m == 'w' && y /= height = Location (x, (y+1))
    | m == 's' && y /= 0      = Location (x, (y-1))
    | m == 'a' && x /= 0      = Location ((x-1), y)      
    | m == 'd' && x /= width  = Location ((x+1), y)
    | otherwise = Location (x,y)
