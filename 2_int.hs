import System.IO
import System.Cmd
import Data.Char
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

height = 30
width =  100
type Grid = UArray (Int, Int) Char

main = do
    hSetBuffering stdin NoBuffering
    gridLoop (0,0)

-- | Grid drawn and player movement processed
gridLoop :: (Int, Int) -> IO()
gridLoop location = do
    system "clear"
    let grid = editGrid location '@' $ buildGrid height width
    putStrLn $ showGrid grid 
    move <- getChar
    let newLocation = updateLocation height width move location
    gridLoop newLocation 


-- | Build a Grid
buildGrid :: Int    --Height
          -> Int    --Width 
          -> Grid   --Grid 
buildGrid h w = runSTUArray $ do 
    let symbols = [c | y <- [0..h], x <- [0..w], let c = getSymbol (y,x)]
    grid <- newListArray ((0,0), (h,w)) symbols
    return grid

-- | Edit the given Grid
editGrid :: (Int, Int) --Cell to change
         -> Char       --New value
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
updateLocation :: Int -> Int -> Char -> (Int, Int) -> (Int, Int)
updateLocation h w m (y, x)
    | m == 'w' && y /= 0     = (y-1, x)
    | m == 's' && y /= h     = (y+1, x)
    | m == 'a' && x /= 0     = (y, x-1)      
    | m == 'd' && x /= w     = (y, x+1)
    | otherwise = (y,x)

-- | Insert string after 
insertEvery :: Int -> Int -> [Char] -> [Char] -> [Char]
insertEvery _ _ _ [x]     = [x]
insertEvery cnt div ins (x:xs) 
    | cnt == div = x : ins ++ (insertEvery 1 div ins xs)
    | otherwise  = x : (insertEvery (cnt + 1) div ins xs)

-- | Get the symbol for a cell
getSymbol :: (Int, Int) -> Char
getSymbol (y,x) 
    | even y && even x = '+'
    | even y && odd x  = '-'
    | odd y && even x  = '|'
    | odd y && odd x   = ' '
