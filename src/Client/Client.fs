module Client

open ClientModel
open ClientFunctions
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


//###############################################################################################################################
//############################################ INIT #############################################################################
//###############################################################################################################################


let init () =
    let fields =  {
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


    let model = {
        //felder
        Moves = fields
        Won = InGame
        Score = 0,0
        BlankFields = 9
        //randsummen
        D1 = 0
        D2 = 0
        D3 = 0
        A4 = 0
        B4 = 0
        C4 = 0
        //randsumme quer
        D4 = 0
        BeginnerMessage = MessageOff
    }
    model, Cmd.none


//#################################################################################################################################
//############################################ UPDATE #############################################################################
//#################################################################################################################################


let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with

    | PCMove ->
        
        //prüfen, ob spieler gewonnen oder unentschieden, wenn gewonnen, reihe einfärben
        if
            checkGameResult model (rowSums (fieldPoints model)) = YouWon ||
            checkGameResult model (rowSums (fieldPoints model)) = Draw
        then playerWonOrDraw model
        //feld nach if bedingung zwingend nicht voll
        else pcMove (rowSums (fieldPoints model)) model

    | ChangeA1 ->
        let neuerZug = {
            model.Moves with 
                A1 = (Some 'O', 1, Neutral)
        }
        playerMove model neuerZug
        
    | ChangeA2 ->
        let neuerZug = {
            model.Moves with 
                A2 = (Some 'O', 1, Neutral)
        }
        playerMove model neuerZug

    | ChangeA3 ->
        let neuerZug = {
            model.Moves with 
                A3 = (Some 'O', 1, Neutral)
        }
        playerMove model neuerZug

    | ChangeB1 ->
        let neuerZug = {
            model.Moves with 
                B1 = (Some 'O', 1, Neutral)
        }
        playerMove model neuerZug

    | ChangeB2 ->
        let neuerZug = {
            model.Moves with 
                B2 = (Some 'O', 1, Neutral) 
        }
        playerMove model neuerZug

    | ChangeB3 ->
        let neuerZug = {
            model.Moves with 
                B3 = (Some 'O', 1, Neutral)  
        }
        playerMove model neuerZug

    | ChangeC1 ->
        let neuerZug = {
            model.Moves with 
                C1 = (Some 'O', 1, Neutral)  
        }
        playerMove model neuerZug

    | ChangeC2 ->
        let neuerZug = {
            model.Moves with 
                C2 = (Some 'O', 1, Neutral)  
        }
        playerMove model neuerZug

    | ChangeC3 ->
        let neuerZug = {
            model.Moves with 
                C3 = (Some 'O', 1, Neutral)  
        }
        playerMove model neuerZug

    | NewGamePlayerBegins -> newGamePlayerBegins model

    | NewGamePCBegins -> newGamePCBegins model

    | SetFieldStatus -> setFieldStatus model 


//###############################################################################################################################
//############################################ VIEW #############################################################################
//###############################################################################################################################


let view (model : Model) (dispatch : Msg -> unit) =
              
    div []
        [
            //Abstand von oben
            Columns.columns
                []
                [
                    Column.column [][]
                ]
            //erste Zeile
            Columns.columns
                []
                [
                    Column.column
                        [Column.Width (Screen.All, Column.Is4)]
                        [
                            yield p
                                [
                                    Style [ CSSProp.FontSize "1.5em"; CSSProp.TextAlign TextAlignOptions.Center ]
                                ]
                                [
                                    str "Regeln:"
                                ]
                            yield p
                                [
                                    Style [ CSSProp.FontSize ".8em"; CSSProp.TextAlign TextAlignOptions.Center  ]
                                ]
                                [
                                    str "Der Verlierer des vorigen Spiels beginnt immer das Neue."
                                ]
                            yield p
                                 [
                                    Style [ CSSProp.FontSize ".8em"; CSSProp.TextAlign TextAlignOptions.Center  ]
                                ]
                                [
                                    str "In allen anderen Fällen wird zufällig gewürfelt, wer anfangen darf."
                                ]
                        ]
                    Column.column
                        [Column.Width (Screen.All, Column.Is4)]
                        [
// ################################################################################################################
// ################################### SPIELFELD ##################################################################
// ################################################################################################################


                            playField

                                //erste Zeile
                                [
                                    Style
                                        [
                                            CSSProp.BorderBottom "1 solid"
                                                             
                                        ]
                                ]
                                [
                                    Button.button
                                        [
                                            //diabled if Won / Lost / Draw
                                            yield setButtonDisabled model
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.A1
                                            yield Button.OnClick (fun _ ->
                                                                        dispatch (ChangeA1)
                                                                        dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.A1 with
                                            | (Some A1, _, _) -> yield str (sprintf "%c" A1)
                                            | (None, _, _) -> yield str ""

                                        ]                                                
                                ]
                            
                                [
                                    Style
                                        [
                                            CSSProp.BorderLeft "1 solid"
                                            CSSProp.BorderRight "1 solid"
                                            CSSProp.BorderBottom "1 solid"
                                        ]
                                ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.A2
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeA2)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.A2 with
                                            | (Some A2, _, _) -> yield str (sprintf "%c" A2)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
                            
                                [ Style [ CSSProp.BorderBottom "1 solid" ] ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model                            
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.A3
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeA3)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.A3 with
                                            | (Some A3, _, _) -> yield str (sprintf "%c" A3)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]

                                //zweite Zeile
                                [
                                    Style
                                        [
                                            CSSProp.BorderBottom "1 solid"
                                        ]
                                ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model                      
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.B1
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeB1)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.B1 with
                                            | (Some B1, _, _) -> yield str (sprintf "%c" B1)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
                            
                                [ Style [ CSSProp.Border "1 solid" ] ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model                                                           
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.B2
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeB2)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.B2 with
                                            | (Some B2, _, _) -> yield str (sprintf "%c" B2)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
                            
                                [
                                    Style
                                        [
                                            CSSProp.BorderBottom "1 solid"
                                        ]
                                ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model                                                       
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.B3
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeB3)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.B3 with
                                            | (Some B3, _, _) -> yield str (sprintf "%c" B3)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]

                                //dritte Zeile
                                [ ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model               
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.C1
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeC1)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.C1 with
                                            | (Some C1, _, _) -> yield str (sprintf "%c" C1)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
                            
                                [
                                    Style
                                        [
                                            CSSProp.BorderLeft "1 solid"
                                            CSSProp.BorderRight "1 solid"
                                        ]
                                ]
                                [
                                    Button.button
                                        [
                                            //yield Button.CustomClass "custom-disabled"
                                            yield setButtonDisabled model                                                          
                                            yield Button.IsFullWidth
                                            yield setButtonColors model.Moves.C2
                                            yield Button.OnClick (fun _ ->
                                                                dispatch (ChangeC2)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.C2 with
                                            | (Some C2, _, _) -> yield str (sprintf "%c" C2)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
                            
                                [ ]
                                [
                                    Button.button
                                        [
                                            yield setButtonDisabled model
                                            yield Button.IsFullWidth
                                                                    
                                            yield setButtonColors model.Moves.C3

                                            yield Button.OnClick (fun e ->
                                                                dispatch (ChangeC3)
                                                                dispatch (PCMove))
                                        ]
                                        [
                                            match model.Moves.C3 with
                                            | (Some C3, _, _) -> yield str (sprintf "%c" C3)
                                            | (None, _, _) -> yield str ""
                                                            
                                        ]
                                ]
// ###########################################################################################################
// #################### PUNKTETABELLE #######################################################################
// ###########################################################################################################
                        ]
                    Column.column
                        [Column.Width (Screen.All, Column.Is4)]
                            [
                                p
                                    [
                                        Style [ CSSProp.FontSize "1.5em"; CSSProp.TextAlign TextAlignOptions.Center  ]
                                    ]
                                    [
                                        str "Punkte"
                                    ]
                                Columns.columns [][
                                    Column.column [ Column.Width (Screen.All, Column.Is4) ][]
                                    Column.column []
                                        [
                                            div
                                                [ Style [CSSProp.TextAlign TextAlignOptions.Center]]
                                                [
                                                    
                                                    scoreTable
                        
                                                        [
                                                            Style [CSSProp.Width "100px"]
                                                        ]
                                                        [ str "Du"]
                                                        []
                                                        [
                                                            let (playerPoints, pcPoints) = model.Score
                                                            yield str (sprintf "%i" playerPoints)
                                                        ]
                                                        []
                                                        [ str "PC"]
                                                                         
                                                        []
                                                        [
                                                            let (playerPoints, pcPoints) = model.Score
                                                            yield str (sprintf "%i" pcPoints)
                                                        ]
                                                
                                                ] //div
                                        ]
                                    Column.column [][]
                                    ]
                                
                            ]

                ]


            Columns.columns
                []
                [
                    Column.column [Column.Width (Screen.All, Column.Is4)]
                        []
                    Column.column [Column.Width (Screen.All, Column.Is4)]
                        [
                            if (model.BlankFields = 9 && model.BeginnerMessage = MessageOff) || model.BlankFields = 0 || model.Won = YouLost || model.Won = YouWon then
                                yield Button.button
                                    [
                                        Button.OnClick (fun _ ->
                                            if model.Won = YouLost then
                                                dispatch NewGamePlayerBegins
                                            elif model.Won = YouWon then
                                                dispatch NewGamePCBegins
                                            else
                                                match r.Next(1,3) with
                                                | 1 -> dispatch NewGamePCBegins
                                                | 2 -> dispatch NewGamePlayerBegins
                                                | _ -> ())
                                    ]
                                    [
                                        str "Neues Spiel?"
                                    ]

                            yield p
                                [ ]
                                [
                                    if model.BeginnerMessage = You then
                                        yield str "Du beginnst."
                                    elif model.BeginnerMessage = PC then
                                        yield str "Der Computer beginnt"
                                    else ()
                                ]



                            
                        ]


                    
                    Column.column
                        [Column.Width (Screen.All, Column.Is4)]
                        [
//################################### TEST ################################################################################
//################################### Randsummen - Tabelle ################################################################

                            //let (summeA4, summeB4, summeC4, summeD0, summeD1, summeD2, summeD3, summeD4) = rowSums (fieldPoints model)              
                            //yield str (sprintf "%A " model.Won)
                            //yield str (sprintf "Blank Fields: %A" model.BlankFields)
                            //yield Table.table
                            //    [ Table.Props [ Style [ CSSProp.Border "1" ] ] ]
                            //    [
                            //        tbody [ ]
                            //            [
                            //                tr
                            //                    [ ]
                            //                    [
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [
                            //                                yield str (sprintf "%i" summeA4)
                            //                            ]

                            //                    ]
                            //                tr
                            //                    [ ]
                            //                    [
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeB4) ]

                            //                    ]
                            //                tr
                            //                    [ ]
                            //                    [
                            //                        td
                            //                            [ ]
                            //                            [ ]                                            
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeC4) ]

                            //                    ]
                            //                tr
                            //                    [ ]
                            //                    [
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeD0) ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeD1) ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeD2) ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeD3) ]
                            //                        td
                            //                            [ ]
                            //                            [ str (sprintf "%i" summeD4) ]

                            //                    ]
                            //            ]
                            //    ]
                                
                        ]
                    
                ]

            

        ]

           

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
