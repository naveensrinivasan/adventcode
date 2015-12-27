open System.IO
let puzzle = File.ReadAllText( Path.Combine(__SOURCE_DIRECTORY__ , "1.txt"))
let runningTotal seq' = (Seq.head seq', Seq.skip 1 seq') ||> Seq.scan (+)
let nums= puzzle |> Seq.map(fun f -> match f.ToString() with
                                                       | "(" -> 1
                                                       | ")" -> -1
                                                       |_ -> 0)

                                                                                                                                                                       
let ``first-1`` = (nums |> runningTotal |> Seq.indexed |> Seq.filter(fun f -> snd(f) = -1 ) |> Seq.map(fun f -> fst f)  |> Seq.head) + 1
                                                      
printfn "Santa would be in %i floor"(nums |> Seq.reduce(+))

printfn "First occurence -1 at position %i " ``first-1``




