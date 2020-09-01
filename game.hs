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
       
game :: Choice -> Choice -> Int -> IO ()
game c1 c2 count = do putStrLn $ "Round: " ++ show count
                      let result = gameLogic c1 c2 
                          winner  | result > 0 = putStrLn "Player1 wins"
                                  | result < 0 = putStrLn "Player2 wins"
                                  | otherwise  = putStrLn "Draw"
                      if count < 5
                      then do winner
                              count <- return (count + 1)
                              game c1 c2 count
                              else putStrLn "game over"

start :: Choice -> Choice -> IO ()
start input1 input2 = game input1 input2 0
