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
gameLogic choice1 choice2 | choice1 == Rock && choice2 == Paper = -1 
                          | choice1 == Rock && choice2 == Scissor = 1
                          | choice1 == Rock && choice2 == Rock = 0 
                          | choice1 == Paper && choice2 == Rock = 1 
                          | choice1 == Paper && choice2 == Scissor = -1
                          | choice1 == Paper && choice2 == Paper = 0 
                          | choice1 == Scissor && choice2 == Rock = -1
                          | choice1 == Scissor && choice2 == Paper = 1 
                          | choice1 == Scissor && choice2 == Scissor = 0 

gameLogic Rock choice | choice == Paper = -1
                      | choice == Scissor = 1
                      | choice == Rock = 0





