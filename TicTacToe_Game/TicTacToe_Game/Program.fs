open System

type Player =  
    |X 
    |O

type Cell = 
    |P of Player
    |Empty

    //
type Status = 
    |Won 
    |Draw
    |InProgress

let switchPlayer (p:Player)  = 
    match p with
    |X -> O
    |O -> X


let hasWon x (grid:Cell [,]) = 
    match grid with
    //horizontal check
    |arr when grid.[0, 0] = x && grid.[0,1] = x && grid.[0, 2] = x || grid.[1, 0] = x && grid.[1, 1] = x && grid.[1, 2] = x || grid.[2, 0] = x && grid.[2, 1] = x && grid.[2, 2] = x -> true
    //vertical checks
    |arr when grid.[0, 0] = x && grid.[1, 0] = x && grid.[2, 0] = x || grid.[0, 1] = x && grid.[1, 1] = x && grid.[2, 1] = x || grid.[0, 2] = x && grid.[1, 2] = x && grid.[2, 2] = x -> true
    //diagonal checks
    |arr when grid.[0, 0] = x && grid.[1, 1] = x && grid.[2, 2] = x || grid.[0, 2] = x && grid.[1, 1] = x && grid.[0, 2] = x -> true
    |_ -> false

let hasDrawn x (grid:Cell [,]) = 
    let available_cells = grid |> Seq.cast<Cell> |> Seq.filter (fun x -> x = Empty)
    Seq.isEmpty available_cells



let checkGridStatus x grid =
    if hasWon x grid then Won
    else if hasDrawn x grid then Draw
    else InProgress

let gameLostByPlayer player =
    printfn "Player %A has lost the game!!" player
    //keep track of score
    

let gameWonByPlayer player = 
    printfn "Player %A has won the game!!" player
    //keep track of score
        //Please insert your name
        //Save score?
    //Other player lost so call that
    gameLostByPlayer (switchPlayer player)


let rec changeGameState player (grid:Cell [,]) = 
    printfn "%A
    
    " grid 
    //Ask user for input
    printfn "Please input the row number"
    let xpos = Console.ReadLine() |> int
    printfn "Please input the column number"
    let ypos = Console.ReadLine() |> int
    //Modify grid
    let addToken xpos ypos (token:Player) (grid:Cell [,]) = grid.[xpos,ypos] <- P token
    addToken xpos ypos player grid
    //Check grid status
    let status = checkGridStatus (P player) grid
    printf "The status is: %A

    " status
    match status with
        |Won -> gameWonByPlayer player
        |Draw -> printfn "Oi! it's a draw!"
        |InProgress -> changeGameState (switchPlayer player) grid

let localGame() = 
    let grid = array2D [[ Empty; Empty; Empty ]
                        [ Empty; Empty; Empty ]
                        [ Empty; Empty; Empty ]] 
    printfn "Working with local game
    
    " 
    changeGameState X grid

let rec onlineGame() = 
    printfn "You have these choices:
  
    1. Host game
    2. Join game

    "
    printf "Please select an  option: "
    match (System.Console.ReadLine() |> int) with
        |1 -> printfn "Host game"
        |2 -> printfn "Join game" //repeat
        |_ -> onlineGame()




let rec playMPGame() = 
    printfn "You have these choices:
  
    1. Local Game
    2. Online Game

    "
    printf "Please select an MP option: "
    match (System.Console.ReadLine() |> int) with
        |1 -> localGame()
        |2 -> onlineGame()
        |_ -> playMPGame()

let playAIGame = ()//printfn "Playing agaisnt AI"


let viewLeaderboard() = printfn "Viewing the leaderboard"
 

let rec gameLoop() =
    printf "Please select an option: "
    match (System.Console.ReadLine() |> int) with
        |1 -> playAIGame
        |2 -> playMPGame() 
        |3 -> viewLeaderboard()
        |_ -> gameLoop()

[<EntryPoint>]
let main argv = 
 
    printfn "Welcome to Tic-Tac Toe:The Game
    You have these choices
    
    1. Play agaisnt AI
    2. Play multiplayer
    3. View Leaderboard
    
    "

    gameLoop()

    0 // return an integer exit code
