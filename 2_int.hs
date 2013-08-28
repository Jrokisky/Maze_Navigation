import System.IO
import System.Cmd
import Data.Char
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed


main = do
    hSetBuffering stdin NoBuffering
    boardLoop (0,0)

--Main Loop where board is redrawn and player movement is processed
boardLoop :: (Int, Int) -> IO()
boardLoop location = do
    system "clear"
    let board = editGrid location '!' $ buildGrid
    print board
    move <- getChar
    let newLocation = updateLocation move location
    boardLoop newLocation 

getSymbol :: (Int, Int) -> Char
getSymbol (x,y) 
    | even x && even y = '+'
    | even x && odd y  = '|'
    | odd x && even y  = '-'
    | odd x && odd y   = ' '

--Make the maze here?
buildGrid :: UArray (Int, Int) Char
buildGrid = runSTUArray $ do 
    let symbols = [c | y <- [0..10], x <- [0..10], let c = getSymbol (x,y)]
    grid <- newListArray ((0,0), (10,10)) symbols
    return grid

editGrid :: (Int, Int) 
         -> Char 
         -> UArray (Int, Int) Char 
         -> UArray (Int, Int) Char
editGrid (x,y) c grid = runSTUArray $ do
    stGrid <- thaw grid
    writeArray stGrid (x,y) 'c'
    return stGrid

--Update Location
updateLocation :: Char -> (Int, Int) -> (Int, Int)
updateLocation m (x, y)
    | m == 'w' && y /= 10     = (x, y+1)
    | m == 's' && y /= 0      = (x, y-1)
    | m == 'a' && x /= 0      = (x-1, y)      
    | m == 'd' && x /= 10     = (x+1, y)
    | otherwise = (x,y)
