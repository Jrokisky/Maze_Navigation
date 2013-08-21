import System.IO
import System.Cmd
import Data.Char

data Board = Board [[Char]] 
data Location = Location (Int, Int)

boardSize = 30

instance Show Board where
    show (Board x) = unlines x

type Size = Int

main = do
    hSetBuffering stdin NoBuffering
    let originalLocation = Location (0,0)
    let originalBoard = createBoard boardSize originalLocation 0 []
    boardLoop originalBoard originalLocation

--Main Loop where board is redrawn and player movement is processed
boardLoop :: Board -> Location -> IO()
boardLoop board location = do
    system "clear"
    putStr $ show board
    move <- getChar
    let newLocation = updateLocation move location
    let newBoard = createBoard boardSize newLocation 0 []
    boardLoop newBoard newLocation 

--Creates a Board
createBoard :: Size -> Location -> Int -> [[Char]] ->  Board
createBoard size (Location (x,y)) row b 
    | row == y     =  createBoard size location nxtRow $ playerRow : b
    | row == size  = Board b
    | otherwise    = createBoard size location nxtRow $ normalRow : b
  where playerRow = map (\z -> if z == x then '@' else '.') [0..(width-1)]
        normalRow = replicate width '.' 
        nxtRow = row + 1
        width = size * 3
        location = Location (x,y)

--Update Location
updateLocation :: Char -> Location -> Location
updateLocation m (Location (x,y)) | m == 'w'  = Location (x, (y+1))
                                  | m == 's'  = Location (x, (y-1))
                                  | m == 'a'  = Location ((x-1), y)
                                  | m == 'd'  = Location ((x+1), y)
                                  | otherwise = Location (x,y)
