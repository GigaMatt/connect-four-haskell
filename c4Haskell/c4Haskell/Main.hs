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

        --we input the pieces into the board
        else do
            putStr (printBoard board)
            putStrLn ("Player " ++ [playerToChar player] ++", pick a column to move:")
            input <- getLine
            --check for if input is empty
            if input == "" then do
                putStrLn "\n--Invalid Input--"
                game board player

            else do
                -- make index 0 = 1
                let slot = (read input)-1

                --check if the input is less than or grater than the bounds
                if slot < 0 || slot >= slotNum board then do
                    putStrLn "\n--Invalid Input--"
                    game board player
                else if not (checkOpenSlot board slot) then do
                    putStrLn "\n--Slot is full!--"
                    game board player
                else game (dropPice board slot player) (togglePlayer player)