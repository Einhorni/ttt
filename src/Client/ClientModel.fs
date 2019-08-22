module ClientModel



type FieldState =
| WinField
| LoseField
| Neutral

type Moves = {
        A1: char option * int * FieldState
        A2: char option * int * FieldState
        A3: char option * int * FieldState
        B1: char option * int * FieldState
        B2: char option * int * FieldState
        B3: char option * int * FieldState
        C1: char option * int * FieldState
        C2: char option * int * FieldState
        C3: char option * int * FieldState
    }

type GameResult =
| InGame
| YouWon
| YouLost
| Draw

type BeginnerMessage =
| You
| PC
| MessageOff

type Model = {
        //felder
        Moves: Moves
        Won: GameResult
        Score: int * int
        BlankFields: int
        //randsummen
        D1: int
        D2: int
        D3: int
        A4: int
        B4: int
        C4: int
        //randsumme quer
        D4: int
        BeginnerMessage: BeginnerMessage
    }


type Msg =
| PCMove
//felder
| ChangeA1 
| ChangeA2 
| ChangeA3 
| ChangeB1
| ChangeB2 
| ChangeB3 
| ChangeC1 
| ChangeC2 
| ChangeC3
| NewGamePlayerBegins
| NewGamePCBegins
| SetFieldStatus
