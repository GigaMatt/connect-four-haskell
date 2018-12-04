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