import System.IO
import System.Cmd
import Data.Char

data Board = Board [[Char]] 
data BoardSize = 20::Int
    
instance Show Board where
    show (Board x) = unlines x

type Size = Int


main = do
    system "clear"
    let board = createBoard BoardSize
    putStr $ show board



--Creates a Board
createBoard :: Size -> Board
createBoard size  = Board $ take size . repeat . take (size * 2) $ repeat '*'

--Set the Player Location
setPlayerLocation :: Int -> Int -> Board -> Board
setPlayerLocation x y oldBoard |
