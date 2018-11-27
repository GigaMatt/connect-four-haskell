{-------------------------------------------
-- Created by samuel Tinevra & Matt Montoya
--------------------------------------------}
module Board where
    import System.IO
    import Data.List

    -- static variable to access the players easier
    playerOne = (1, 'X')--, "Player 1"]
    playerTwo = (2, 'O')--, "Player 2"]

    togglePlayer player = if player == 1 then 2 else 1 -- Switch player

    -- Makes a new connect 4 board
    board :: Int -> Int -> [[Int]]
    board m n = replicate m (replicate n 0)
    