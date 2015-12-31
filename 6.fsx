open System.IO
open System.Text.RegularExpressions


(**

--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

turn on 0,0 through 999,999 would turn on (or leave on) every light.
toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
After following the instructions, how many lights are lit?
**)

let filereadlines f = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, f ))

type Switch = On | Off | Toggle

type Instruction = {
  State : Switch;
  StartRow  : int;
  StartCol : int;
  EndRow : int;
  EndCol : int
}

let lights =Array2D.create 1000 1000 Off

let toggle = function
            | On -> Off
            | Off -> On

let getInstruction (line:string)= 
                  let matches = Regex.Matches(line, "[\w\d_]+")
                                        |> Seq.cast<Match>
                                        |> Seq.filter (fun f -> f.Success) |> Seq.map(fun f-> f.Value) 

                  let rowstate = if matches |> Seq.nth 0 = "toggle" then Toggle
                                  elif matches |> Seq.nth 1 = "on"   then  On
                                  else Off 
                  let elementat n= int(matches |> Seq.nth n)
                  match rowstate with
                  |Toggle -> {State = rowstate; StartRow = elementat 1; StartCol = elementat 2; 
                                EndRow = elementat 4 ; EndCol = elementat 5}
                  |_ -> {State = rowstate; StartRow = elementat 2; StartCol = elementat 3;
                          EndRow = elementat 5 ; EndCol = elementat 6} 

"6.txt"
|> filereadlines
|> Seq.map getInstruction
|> Seq.iter(fun ins -> 
        for i in  ins.StartRow .. ins.EndRow do
            for j in ins.StartCol .. ins.EndCol  do
                match ins.State with
                |Toggle ->  lights.[i, j] <- (toggle (lights.[i,j]))
                |_ ->  lights.[i, j] <- ins.State  
        ) |> ignore

printfn "Lights that were turned on are %i"  (lights |> Seq.cast<Switch> |> Seq.filter(fun f -> f = On) |> Seq.length) 