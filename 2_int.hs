import System.IO
import System.Cmd
import Data.Char

data Board = Board [[Char]] 
data Location = Location (Int, Int)

boardSize = 20
    
instance Show Board where
    show (Board x) = unlines x

type Size = Int

main = do
    system "clear"
    let board = createBoard boardSize (Location (5,10)) 0 []
    putStr $ show board


--Creates a Board
createBoard :: Size -> Location -> Int -> [[Char]] ->  Board
createBoard size (Location (x,y)) row b | row == y     =  createBoard size (Location (x,y)) (row + 1) (map filter [1..(size*2)] : b)
                                        | row == size  = Board (b)
                                        | otherwise    = createBoard size (Location (x,y)) (row + 1) (replicate (size*2) '*' : b)
                                  where filter = (\z -> if z == x then '@' else '*')

