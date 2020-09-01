--- Implementing a game of Rock, Paper and Scissors
data Choice = Start | Rock | Paper | Scissor
            deriving (Eq, Show, Read)
type PName = String
data Player = Player { playerName :: PName
                     , playerChoice :: Choice }
            deriving (Show)

veera :: Player
veera = Player "Veera" Start 

computer :: Player
computer = Player "AI" Start 

gameLogic :: Choice -> Choice -> Int
gameLogic Rock choice | choice == Paper = -1
                      | choice == Scissor = 1
                      | choice == Rock = 0

gameLogic Paper choice | choice == Rock = 1
                       | choice == Scissor = -1
                       | choice == Paper = 0

gameLogic Scissor choice | choice == Rock = -1
                         | choice == Paper = 1
                         | choice == Scissor = 0

options :: [Choice]
options = [Rock, Paper, Scissor]
       
game :: Choice -> Choice -> IO ()
game c1 c2 = do putStrLn "Game Begins"
            let result = gameLogic c1 c2 
                    winner  | result > 0 = putStrLn "Player1 wins"
                            | result < 0 = putStrLn "Player2 wins"
                            | otherwise  = putStrLn "Draw"
                winner

