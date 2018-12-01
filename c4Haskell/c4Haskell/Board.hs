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

    -- prints board
    printBoard :: [[Int]] -> String
    printBoard board = "\n" ++ boardToStrHelper board 0 ++ "\n"

    -- Converts the player number to a string character for board printing
    playerToChar :: Int -> Char
    playerToChar p
        | p == 1 = '1'
        | p == 2 = '2'
        | otherwise = '?' -- Empty space


    --    helper methods for checkWon

        -- Check rows and colums
        checkCardinals :: [[Int]] -> Int -> Bool
        checkCardinals board player = checkColums board player 0
                                        || checkRows board player 0
             
                                        
    -- check rows
    checkRows :: [[Int]] -> Int -> Int -> Bool
    checkRows board player y
        | y >= boardHeight board = False
        | checkRow board player (0, y) 0 = True
        | otherwise = checkRows board player (y+1)

    -- Check individual row for winning player
    checkRow :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
    checkRow board player sqr count
        | count >= 4 = True
        | fst sqr >= slotNum board = False
        | player == board!!(snd sqr)!!(fst sqr) = checkRow board player (fst sqr+1, snd sqr) (count+1)
        | otherwise = checkRow board player (fst sqr+1, snd sqr) 0

    -- -- check column
    checkColums :: [[Int]] -> Int -> Int -> Bool
    checkColums board player x
        | x >= slotNum board = False
        | checkOneColumn board player (x, 0) 0 = True
        | otherwise = checkColums board player (x+1)

    -- Check individual column for winning player
    checkOneColumn :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
    checkOneColumn board player sqr count
        | count >= 4 = True
        | snd sqr >= boardHeight board = False
        | player == board!!(snd sqr)!!(fst sqr) = checkOneColumn board player (fst sqr, snd sqr+1) (count+1)
        | otherwise = checkOneColumn board player (fst sqr, snd sqr+1) 0



    -- Check diagonals
    checkDiagonals :: [[Int]] -> Int -> Bool
    checkDiagonals board player =  forwslashUpper board player 0
                                || forwslashLower board player 0
                                || backslashUpper board player 0
                                || backslashLower board player 0

    -- Checks the upper half
    forwslashUpper :: [[Int]] -> Int -> Int -> Bool
    forwslashUpper board player y
        | y >= boardHeight board = False
        | checkForwslash board player (0,y) 0 = True
        | otherwise = forwslashUpper board player (y+1)

    -- Checks the bottom
    forwslashLower :: [[Int]] -> Int -> Int -> Bool
    forwslashLower board player x
        | x >= slotNum board = False
        | checkForwslash board player (x, boardHeight board-1) 0 = True
        | otherwise = forwslashLower board player (x+1)
        