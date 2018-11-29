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
    
    -- Fills the desired board position
    dropPice :: [[Int]] -> Int -> Int -> [[Int]]
    dropPice [] _ _ = [[]]
    dropPice boardDimension i p = do
        if i < 0 || i >= slotNum boardDimension || not (checkOpenSlot boardDimension i) then []
        else dropEmpty boardDimension p ((boardHeight boardDimension)-1) i
    
    -- Checks if board slot is open
        checkOpenSlot :: [[Int]] -> Int -> Bool
        checkOpenSlot [[]] _ = False
        checkOpenSlot boardDimension i = if boardDimension!!0!!i == 0 then True else False

    -- sees the number of slots empty lefts
    slotNum :: [[Int]] -> Int
    slotNum boardDimension = length (boardDimension!!0)

    -- checks if theres a empty slot
    isDraw :: [[Int]] -> Bool
    isDraw boardDimension = isBoardFull boardDimension 0

    -- check if someone won
    checkWon :: [[Int]] -> Int -> Bool
    checkWon boardDimension p = checkCardinals boardDimension p || checkDiagonals boardDimension p
