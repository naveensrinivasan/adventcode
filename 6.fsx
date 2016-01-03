open System.IO
open System.Text.RegularExpressions

let filereadlines f = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, f))

type Switch = 
    | On
    | Off
    | Brightness of int

type Action = 
    | On
    | Off
    | Toggle

type MessageLanguage = 
    | Nordic
    | English

type Instruction = 
    { Operation : Action
      StartRow : int
      StartCol : int
      EndRow : int
      EndCol : int }

let toggle = 
    function 
    | Switch.On -> Switch.Off
    | _ -> Switch.On

let getInstruction (line : string) = 
    let matches = 
        Regex.Matches(line, "[\w\d_]+")
        |> Seq.cast<Match>
        |> Seq.filter (fun f -> f.Success)
        |> Seq.map (fun f -> f.Value)
    
    let action = 
        if matches
           |> Seq.nth 0
           = "toggle" then Toggle
        elif matches
             |> Seq.nth 1
             = "on" then On
        else Off
    
    let elementat n = 
        matches
        |> Seq.nth n
        |> int
    
    match action with
    | Toggle -> 
        { Operation = action
          StartRow = elementat 1
          StartCol = elementat 2
          EndRow = elementat 4
          EndCol = elementat 5 }
    | _ -> 
        { Operation = action
          StartRow = elementat 2
          StartCol = elementat 3
          EndRow = elementat 5
          EndCol = elementat 6 }

let translateCode (lang : MessageLanguage) (action : Action) (state : Switch) = 
    match lang, action, state with
    | English, Toggle, _ -> toggle state
    | English, On, _ -> Switch.On
    | Nordic, Toggle, Brightness(x) -> Brightness(x + 2)
    | Nordic, On, Brightness(x) -> Brightness(x + 1)
    | Nordic, Off, Brightness(x) -> Brightness(max (x - 1) 0)
    | _, _, _ -> Switch.Off

let followInstructions (language : MessageLanguage) (lights : Switch [,]) = 
    let translate = translateCode language
    "6.txt"
    |> filereadlines
    |> Seq.map getInstruction
    |> Seq.iter (fun ins -> 
           for i in ins.StartRow..ins.EndRow do
               for j in ins.StartCol..ins.EndCol do
                   lights.[i, j] <- translate ins.Operation lights.[i, j])

let lights = Array2D.create 1000 1000 Switch.Off
let nordiclights = Array2D.create 1000 1000 (Brightness 0)

followInstructions English lights
followInstructions Nordic nordiclights

lights
|> Seq.cast<Switch>
|> Seq.filter (fun f -> f = Switch.On)
|> Seq.length
|> printfn "The number of lights that are turned on are %i"

nordiclights
|> Seq.cast<Switch>
|> Seq.map (fun f -> 
       match f with
       | Brightness(x) -> x
       | _ -> 0)
|> Seq.sum
|> printfn 
       "The Santa's real nordic decoded message and the total brightness is %i"
