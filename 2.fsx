open System
open System.IO

type Rect = {
  Length : int;
  Width  : int;
  Height : int
}

let parse (s:string) =
  let splitstring = s.Split([|'x'|])
  {Length = int splitstring.[0]; Width = int splitstring.[1]; Height = int splitstring.[2]}

let prismarea (r:Rect) =
  let calculations = [(2 * r.Length * r.Width); (2 * r.Width * r.Height); (2 * r.Height * r.Length)] 
  let lowest = (calculations |> List.min) / 2
  lowest::calculations |> List.reduce(+) 

let area = parse >> prismarea

let totalarea =File.ReadAllLines (Path.Combine(__SOURCE_DIRECTORY__, "2.txt")) |> Seq.map(fun f-> area f) |> Seq.reduce(+)

printfn "Total area required is %i" totalarea 


let ribbonLength (r:Rect) =
        let list = [|r.Height;r.Width;r.Length|]
        let bow = list |> Array.reduce (*)
        let [|l;w|] =  Array.sort(list).[0..1]
        let wrap = (l + l) + (w+w) 
        bow+wrap

let ribbonsize = parse >> ribbonLength
let totalRibbonLength =File.ReadAllLines (Path.Combine(__SOURCE_DIRECTORY__, "2.txt")) |> Seq.map(fun f-> ribbonsize f) |> Seq.reduce(+)

printfn "Total Ribbon length required is %i " totalRibbonLength
