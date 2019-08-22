
module Logic

open System
//wenn erstes Symbol x ist, muss nächstes Symbol o werden und andersherum
//wenn spieler gesetzt hat, muss computer zufällig woanders setzen (mit anderem Symbol)
    //herausfinden, wo spieler gesetzt hat = da wo feld = Some

let Randomize s = 
   let r = new Random()
   s |> List.sortBy (fun _ -> r.Next())

let fields = [None; None; Some 'x'; Some 'o'; None]

let randomCalculateNextStep (fields: char option list) =
    let onlyNoneFields =
        fields
        |> List.filter (fun x -> x = None)
    let randomField =
        onlyNoneFields
        |> Randomize
        |> List.head
    (randomField, 'x')



 


