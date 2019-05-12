namespace TicTacToe_Game
module Error =
    type Error =
        |BlankError
        |ParseError 
        static member GetErrorMessage x =
            match x with
            |BlankError -> 
                sprintf "ERROR: The input can not be empty."
