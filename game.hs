--- Implementing a game of Rock, Paper and Scissors
data Choice = Start | Rock | Paper | Scissor
            deriving (Eq, Show)
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





