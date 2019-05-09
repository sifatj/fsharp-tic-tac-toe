// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Player =  
    |X 
    |O

type Cell = 
    |P of Player
    |Empty

//let (|Cell|) (P x, Empty) = Cell Player
type Status = 
    |Won
    |Draw
    |Progress



let switchPlayer (p:Player)  = 
    match p with
    |X -> O
    |O -> X


let localGame() = 
    let grid = array2D [[ Empty; Empty; Empty ]
                        [ Empty; Empty; Empty ]
                        [ Empty; Empty; Empty ]] 
    0
         

    //Let player choose
    //Make change to choose
    //Modify grid
    //Check new grid
    //Repeat with new player

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

let playAIGame = printfn "Playing agaisnt AI"


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
