open System.IO
let puzzle = File.ReadAllText( Path.Combine(__SOURCE_DIRECTORY__ , "1.txt")) 
let nums= puzzle |> Seq.map(fun f -> match f.ToString() with
                                                       | "(" -> 1
                                                       | ")" -> -1
                                                       |_ -> 0) |> Seq.reduce(+)
 printfn "%i " nums