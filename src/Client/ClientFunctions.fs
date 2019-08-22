module ClientFunctions

open ClientModel
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared
open System
open Thoth.Json


let r = new Random()

//punkte auf den feldern
let fieldPoints (model:Model) =
    let (_, a1, _) = model.Moves.A1
    let (_, a2, _) = model.Moves.A2
    let (_, a3, _) = model.Moves.A3
    let (_, b1, _) = model.Moves.B1
    let (_, b2, _) = model.Moves.B2
    let (_, b3, _) = model.Moves.B3
    let (_, c1, _) = model.Moves.C1
    let (_, c2, _) = model.Moves.C2
    let (_, c3, _) = model.Moves.C3
    (a1, a2, a3, b1, b2, b3, c1, c2, c3)


//randsummen
let rowSums fieldPoints =
    let (a1, a2, a3, b1, b2, b3, c1, c2, c3) = fieldPoints
    let summeA4 = a1 + a2 + a3
    let summeB4 = b1 + b2 + b3
    let summeC4 = c1 + c2 + c3
    let summeD0 = c1 + b2 + a3
    let summeD1 = a1 + b1 + c1
    let summeD2 = a2 + b2 + c2
    let summeD3 = a3 + b3 + c3
    let summeD4 = a1 + b2 + c3
    (summeA4, summeB4, summeC4, summeD0, summeD1, summeD2, summeD3, summeD4)


// randsummen = 3 / 30, hat spielder oder Pc gewonnen oder noch mitten im Spiel?
let checkGameResult (model:Model) points =
    let (summeA4, summeB4, summeC4, summeD0, summeD1, summeD2, summeD3, summeD4) = points
    if summeA4 = 3 || summeB4 = 3 || summeC4 = 3 || summeD0 = 3 || summeD1 = 3 || summeD2 = 3 || summeD3 = 3 || summeD4 = 3 then
        YouWon
        
    elif summeA4 = 30 || summeB4 = 30 || summeC4 = 30 || summeD0 = 30 ||  summeD1 = 30 || summeD2 = 30 || summeD3 = 30 || summeD4 = 30 then
        YouLost
    elif (summeA4 <> 30 || summeB4 <> 30 || summeC4 <> 30 || summeD0 <> 30 ||  summeD1 <> 30 || summeD2 <> 30 || summeD3 <> 30 || summeD4 <> 30) && model.BlankFields = 0 then
        Draw
    else InGame


//spielerzug
let playerMove model neuerZug =
    let newBlankFields = model.BlankFields - 1
    let neuesModel = {
        model with
            Moves = neuerZug
            BlankFields = newBlankFields
            BeginnerMessage = MessageOff
    }
    neuesModel, Cmd.none


//Status NACH PC-Zug, ob pc gewonnen, unentschieden oder keine Änderung = inGame
let tryPlayerLost model =   
    if checkGameResult model (rowSums (fieldPoints model)) = YouLost then
        let (player, pc) = model.Score
        let pcPoints = pc + 1
        let neuesModel = {
            model with
                Won = YouLost
                Score = (player, pcPoints)
        }
        // Feldstatus der X-reihe auf Verloren stellen
        let setFieldCmd =
            Cmd.ofMsg SetFieldStatus
        neuesModel, setFieldCmd
    elif checkGameResult model (rowSums (fieldPoints model)) = Draw then
        let neuesModel = {
            model with
                Won = Draw
            }
        neuesModel, Cmd.none
    else model, Cmd.none




//prüfung spieler gewonnen oder unentschieden zu BEGINN pc zug, model evtl ändern
let playerWonOrDraw model =
    if checkGameResult model (rowSums (fieldPoints model)) = YouWon then
        let (player, pc) = model.Score
        let playerPoints = player + 1
        let neuesModel = {
            model with
                Won = YouWon
                Score = (playerPoints, pc)
        }
        //feld status der o-reihe auf gewonnen stellen
        let setfieldCmd =
            Cmd.ofMsg SetFieldStatus
        neuesModel, setfieldCmd
    else
        let neuesModel = {
            model with
                Won = Draw
            }
        neuesModel, Cmd.none

let setFieldStatus model =
    let (summeA4, summeB4, summeC4, summeD0, summeD1, summeD2, summeD3, summeD4) = rowSums (fieldPoints model)

    if summeA4 = 30 then
        let newFields = {
            model.Moves with
                A1 = (Some 'X', 10, LoseField)
                A2 = (Some 'X', 10, LoseField)
                A3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeA4 = 3 then
        let newFields = {
            model.Moves with
                A1 = (Some 'O', 1, WinField)
                A2 = (Some 'O', 1, WinField)
                A3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeB4 = 30 then
        let newFields = {
            model.Moves with
                B1 = (Some 'X', 10, LoseField)
                B2 = (Some 'X', 10, LoseField)
                B3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeB4 = 3 then
        let newFields = {
            model.Moves with
                B1 = (Some 'O', 1, WinField)
                B2 = (Some 'O', 1, WinField)
                B3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeC4 = 30 then
        let newFields = {
            model.Moves with
                C1 = (Some 'X', 10, LoseField)
                C2 = (Some 'X', 10, LoseField)
                C3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeC4 = 3 then
        let newFields = {
            model.Moves with
                C1 = (Some 'O', 1, WinField)
                C2 = (Some 'O', 1, WinField)
                C3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD0 = 30 then
        let newFields = {
            model.Moves with
                C1 = (Some 'X', 10, LoseField)
                B2 = (Some 'X', 10, LoseField)
                A3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD0 = 3 then
        let newFields = {
            model.Moves with
                C1 = (Some 'O', 1, WinField)
                B2 = (Some 'O', 1, WinField)
                A3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD1 = 30 then
        let newFields = {
            model.Moves with
                A1 = (Some 'X', 10, LoseField)
                B1 = (Some 'X', 10, LoseField)
                C1 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD1 = 3 then
        let newFields = {
            model.Moves with
                A1 = (Some 'O', 1, WinField)
                B1 = (Some 'O', 1, WinField)
                C1 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD2 = 30 then
        let newFields = {
            model.Moves with
                A2 = (Some 'X', 10, LoseField)
                B2 = (Some 'X', 10, LoseField)
                C2 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD2 = 3 then
        let newFields = {
            model.Moves with
                A2 = (Some 'O', 1, WinField)
                B2 = (Some 'O', 1, WinField)
                C2 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD3 = 30 then
        let newFields = {
            model.Moves with
                A3 = (Some 'X', 10, LoseField)
                B3 = (Some 'X', 10, LoseField)
                C3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD3 = 3 then
        let newFields = {
            model.Moves with
                A3 = (Some 'O', 1, WinField)
                B3 = (Some 'O', 1, WinField)
                C3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD4 = 30 then
        let newFields = {
            model.Moves with
                A1 = (Some 'X', 10, LoseField)
                B2 = (Some 'X', 10, LoseField)
                C3 = (Some 'X', 10, LoseField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
    elif summeD4 = 3 then
        let newFields = {
            model.Moves with
                A1 = (Some 'O', 1, WinField)
                B2 = (Some 'O', 1, WinField)
                C3 = (Some 'O', 1, WinField)
        }
        let neuesModel = {
            model with
                Moves = newFields
        }
        neuesModel, Cmd.none
            
    else model, Cmd.none


// pc zug
let pcMove sumsOfRows model =
    let (summeA4, summeB4, summeC4, summeD0, summeD1, summeD2, summeD3, summeD4) = sumsOfRows

    //blocken, wenn spieler 2 in einer reihe
    if summeA4 = 2
    then
        if model.Moves.A1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            //model, Cmd mit Prüfung, ob Spieler verloren, unentschieden oder keine Änderun g= Ingame
            tryPlayerLost neuesModel
            
        elif model.Moves.A2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.A3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            //hier kann ich auch model, Cmd.none schreiben - der fall tritt nie ein, wenn randsumme = 2 ist immer ein feld frei
            tryPlayerLost neuesModel
        else
            model, Cmd.none
    elif summeB4 = 2
    then
        if model.Moves.B1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeC4 = 2
    then
        if model.Moves.C1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.C2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.C3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeD0 = 2
    then
        if model.Moves.C1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.A3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeD1 = 2
    then
        if model.Moves.A1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.C1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeD2 = 2
    then
        if model.Moves.A2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.C2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeD3 = 2
    then
        if model.Moves.C3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.A3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model

    elif summeD4 = 2
    then
        if model.Moves.A1 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    A1 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.B2 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    B2 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        elif model.Moves.C3 = (None,0,Neutral)
        then
            let newFields = {
                model.Moves with
                    C3 = (Some 'X', 10, Neutral)
            }
            let newBlankFields = model.BlankFields - 1
            let neuesModel = {
                model with
                    Moves = newFields
                    BlankFields = newBlankFields
            }
            tryPlayerLost neuesModel
        else tryPlayerLost model
    else
        //zufälliger zug             
        let rec calculateNextMove (r:Random) =
            match r.Next(1,10) with
            | 1 ->
                //prüfen, ob Feld schon gefüllt
                if model.Moves.A1 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            A1 = (Some 'X', 10, Neutral)
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }
                    neuesModel
                else calculateNextMove r
            | 2 ->
                if model.Moves.A2 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            A2 = (Some 'X', 10, Neutral)  
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }        
                    neuesModel
                else calculateNextMove r
            | 3 ->
                if model.Moves.A3 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            A3 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }   
                    neuesModel
                else calculateNextMove r
            | 4 ->
                if model.Moves.B1 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            B1 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }
                    neuesModel
                else calculateNextMove r
            | 5 ->
                if model.Moves.B2 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            B2 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }        
                    neuesModel
                else calculateNextMove r
            | 6 ->
                if model.Moves.B3 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            B3 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }        
                    neuesModel
                else calculateNextMove r
            | 7 ->
                if model.Moves.C1 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            C1 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }       
                    neuesModel
                else calculateNextMove r
            | 8 ->
                if model.Moves.C2 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            C2 = (Some 'X', 10, Neutral)   
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }
                    neuesModel
                else calculateNextMove r
            | 9 ->
                if model.Moves.C3 = (None, 0, Neutral) then
                    let neuerZug = {
                        model.Moves with
                            C3 = (Some 'X', 10, Neutral)  
                    }
                    let newBlankFields = model.BlankFields - 1
                    let neuesModel = {
                        model with
                            Moves = neuerZug
                            BlankFields = newBlankFields
                    }
                    neuesModel
                else calculateNextMove r
            | _ -> model
        
        let neuesModel = calculateNextMove r
        //model, Cmd mit Prüfung, ob Spieler verloren, unentschieden oder keine Änderun g= Ingame          
        tryPlayerLost neuesModel


let newGamePlayerBegins model =
    let leeresFeld = {
        model.Moves with
            A1 = (None, 0, Neutral)
            A2 = (None, 0, Neutral)
            A3 = (None, 0, Neutral)
            B1 = (None, 0, Neutral)
            B2 = (None, 0, Neutral)
            B3 = (None, 0, Neutral)
            C1 = (None, 0, Neutral)
            C2 = (None, 0, Neutral)
            C3 = (None, 0, Neutral)
    }
    let neuesModel = {
        model with
            Moves = leeresFeld
            Won = InGame
            BlankFields = 9
            BeginnerMessage = You
    }
    neuesModel, Cmd.none

let newGamePCBegins model =
    let leeresFeld = {
        model.Moves with
            A1 = (None, 0, Neutral)
            A2 = (None, 0, Neutral)
            A3 = (None, 0, Neutral)
            B1 = (None, 0, Neutral)
            B2 = (None, 0, Neutral)
            B3 = (None, 0, Neutral)
            C1 = (None, 0, Neutral)
            C2 = (None, 0, Neutral)
            C3 = (None, 0, Neutral)
    }
    let msgPCMove =Cmd.ofMsg PCMove
    let neuesModel = {
        model with
            Moves = leeresFeld
            Won = InGame
            BlankFields = 9
            BeginnerMessage = PC
    }
    neuesModel, msgPCMove


//hier werden die gewinnbuttons eingefärbt
let setButtonColors (field: char option * int * FieldState) =
    match field with
    | (_, _, WinField) ->
        Button.Props [
            yield Style [
                Border "0"; BoxShadow "0px #ffffff"
                Color "#00FF00"] ]
    | (_, _, LoseField) ->
        Button.Props [
            yield Style [
                Border "0"; BoxShadow "0px #ffffff"
                Color "#FF0000"] ]
    | (_, _, Neutral) ->
        Button.Props [
            yield Style [
                Border "0"; BoxShadow "0px #ffffff"
                Color "#000000"] ]


//zu beginn, nach Draw, gewonne, verloren auf diabled setzen
let setButtonDisabled (model:Model) =
    Button.Disabled (model.Won = YouWon || model.Won = YouLost || model.Won = Draw || (model.BeginnerMessage = MessageOff && model.BlankFields = 9))


let playField tdProps1 tdContent1 tdProps2 tdContent2 tdProps3 tdContent3 tdProps4 tdContent4 tdProps5 tdContent5 tdProps6 tdContent6 tdProps7 tdContent7 tdProps8 tdContent8  tdProps9 tdContent9 =
    Table.table
        [ Table.IsFullWidth ]
        [
            tbody
                []
                [
                    tr
                        [  ]
                        [
                            td tdProps1 tdContent1 
                            td tdProps2 tdContent2
                            td tdProps3 tdContent3 
                        ]
                    tr
                        [  ]
                        [
                            td tdProps4 tdContent4 
                            td tdProps5 tdContent5
                            td tdProps6 tdContent6
                        ]
                    tr
                        [  ]
                        [
                            td tdProps7 tdContent7 
                            td tdProps8 tdContent8
                            td tdProps9 tdContent9 
                        ]    
                ]
        ]




let scoreTable tdProps1 tdContent1 tdProps2 tdContent2 tdProps3 tdContent3 tdProps4 tdContent4 =
    Table.table
        [ Table.IsFullWidth ]
        [
            tbody
                []
                [
                    tr
                        [  ]
                        [
                            td tdProps1 tdContent1 
                            td tdProps2 tdContent2 
                        ]
                    tr
                        [  ]
                        [
                            td tdProps3 tdContent3 
                            td tdProps4 tdContent4 
                        ]
                        
                ]
        ]