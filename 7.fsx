open System
open System.Text.RegularExpressions

type Exp = 
    | ID of string * Int16
    | NOT of string * string
    | AND of string * string * string
    | OR of string * string * string
    | LSHIFT of string * int * string
    | RSHIFT of string * int * string

let inputs = 
    [ "123 -> x"; "456 -> y"; "x AND y -> d"; "x OR y -> e"; "x LSHIFT 2 -> f"; 
      "y RSHIFT 2 -> g"; "NOT x -> h"; "NOT y -> i" ]

let parse (line : string) = 
    let words = 
        Regex.Matches(line, "[\w\d_]+")
        |> Seq.cast<Match>
        |> Seq.filter (fun f -> f.Success)
        |> Seq.map (fun f -> f.Value)
        |> Seq.toArray
    match words with
    | [| a; b |] -> ID(b, int16 a)
    | [| a; b; c |] -> NOT(b, c)
    | [| a; b; c; d |] when b = "AND" -> AND(a, c, d)
    | [| a; b; c; d |] when b = "OR" -> OR(a, c, d)
    | [| a; b; c; d |] when b = "LSHIFT" -> LSHIFT(a, int c, d)
    | [| a; b; c; d |] when b = "RSHIFT" -> RSHIFT(a, int c, d)
    | _ -> failwith "Pattern not found"

let identifiers (exps : seq<Exp>) = 
    exps
    |> Seq.map (fun f -> 
           match f with
           | ID(a, b) -> [ a ]
           | NOT(a, b) -> [ a; b ]
           | AND(a, b, c) -> [ a; b; c ]
           | OR(a, b, c) -> [ a; b; c ]
           | LSHIFT(a, _, b) -> [ a; b ]
           | RSHIFT(a, _, b) -> [ a; b ])
    |> Seq.collect (fun f -> f)
    |> Seq.distinct
    |> Seq.map (fun f -> (f, int16 0))
    |> Map.ofSeq

let processExp (exp : Exp) (ids : Map<string, int16>) = 
    match exp with
    | ID(a, b) -> ids.Remove(a).Add(a, b)
    | NOT(a, b) -> ids.Remove(b).Add(b, (~~~ids.[a]))
    | AND(a, b, c) -> ids.Remove(c).Add(c, (ids.[a] &&& ids.[b]))
    | OR(a, b, c) -> ids.Remove(c).Add(c, (ids.[a] ||| ids.[b]))
    | LSHIFT(a, b, c) -> ids.Remove(c).Add(c, (ids.[a] <<< b))
    | RSHIFT(a, b, c) -> ids.Remove(c).Add(c, (ids.[a] >>> b))

let parsedInputs = inputs |> Seq.map (fun f -> parse f)
let map = parsedInputs |> identifiers
