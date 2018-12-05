{-------------------------------------------
-- Created by samuel Tinevra & Matt Montoya
--------------------------------------------}

module Main where
    import System.IO
    import Board

    main = do
        --set the board to the dimension 6 and 7
        game (board 6 7) 1

    -- recursive call until someone wins the game
    game :: [[Int]] -> Int -> IO()
    game [[]] _ = putStrLn "Invalid Input, exiting"
    game board player = do
        let prevPlayer = togglePlayer player -- Prev player
        --check if a player won
        if checkWon board prevPlayer then do
            putStr (printBoard board)
            putStrLn ("Player " ++ [playerToChar prevPlayer] ++ " wins!!!")

        --check for a draw
        else if isDraw board then putStrLn "\nIt's a draw."