namespace TicTacToe_Game
module Error =
    type Error =
        |BlankError
        |ParseError of string
        |RunCommandError
        static member GetErrorMessage x =
            match x with
            |BlankError -> 
                sprintf "ERROR: The input can not be empty."
            |ParseError x -> 
                sprintf "ERROR: Could not parse %s." x
            |RunCommandError -> 
                sprintf "ERROR: There was a problem with running the command"
