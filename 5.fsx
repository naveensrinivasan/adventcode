#I __SOURCE_DIRECTORY__
#load "settings.fsx"

open adventcode

let vowels = "aeiou".ToCharArray() |> Array.map int
let ignorelist = [|"ab";"cd";"pq";"xy"|] |> Array.map (fun f -> int (f.ToCharArray().[0]) , int(f.ToCharArray().[1])) 

let occuredmorethanonce (l:seq<int>) = l |> Seq.pairwise  |> Seq.exists(fun f -> fst f = snd f)
let atleast3vowels (l:seq<int>) = l |> Seq.filter(fun f -> vowels |> Seq.exists(fun v -> v = f)) |> Seq.length > 2
let isnotpartofigorelist (l:seq<int>) = l |> Seq.pairwise |> Seq.exists(fun f -> (ignorelist |> Seq.exists(fun i -> i = f))) |> not


let nicestring (s:string) =
    let numberlist = s.ToCharArray() |> Array.map int 
    (atleast3vowels numberlist) && (occuredmorethanonce numberlist) && (isnotpartofigorelist numberlist)

"5.txt"
|> filereadlines
|> Seq.map nicestring 
|> Seq.filter (fun f -> f = true)
|> Seq.length 
|> printfn "Number of nice strings are %i " 
 
