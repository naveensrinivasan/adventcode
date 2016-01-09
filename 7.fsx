open System
open System.IO
open System.Text.RegularExpressions

let filereadlines f = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, f))

type Operations = 
    | ASSIGNVALUE
    | ASSIGN of string
    | AND of string * string
    | INTAND of uint16 * string
    | OR of string * string
    | INTOR of uint16 * string
    | NOT of string
    | LSHIFT of int * string
    | RSHIFT of int * string

type Instruction = 
    { Key : string
      Value : uint16 option
      Operation : Operations }

let parse (line : string) = 
    let words = 
        Regex.Matches(line, "[\w\d_]+")
        |> Seq.cast<Match>
        |> Seq.filter (fun f -> f.Success)
        |> Seq.map (fun f -> f.Value)
        |> Seq.toArray
    match words with
    | [| a; b |] when Int16.TryParse a |> fst -> 
        { Key = b
          Value = uint16 a |> Some
          Operation = Operations.ASSIGNVALUE }
    | [| a; b |] -> 
        { Key = b
          Value = None
          Operation = Operations.ASSIGN(a) }
    | [| a; b; c |] when a = "NOT" -> 
        { Key = c
          Value = None
          Operation = Operations.NOT(b) }
    | [| a; b; c; d |] when b = "AND" && Int16.TryParse a |> fst -> 
        { Key = d
          Value = None
          Operation = Operations.INTAND(uint16 a, c) }
    | [| a; b; c; d |] when b = "AND" -> 
        { Key = d
          Value = None
          Operation = Operations.AND(a, c) }
    | [| a; b; c; d |] when b = "OR" && Int16.TryParse a |> fst -> 
        { Key = d
          Value = None
          Operation = Operations.INTOR(uint16 a, c) }
    | [| a; b; c; d |] when b = "OR" -> 
        { Key = d
          Value = None
          Operation = Operations.OR(a, c) }
    | [| a; b; c; d |] when b = "LSHIFT" -> 
        { Key = d
          Value = None
          Operation = Operations.LSHIFT(int c, a) }
    | [| a; b; c; d |] when b = "RSHIFT" -> 
        { Key = d
          Value = None
          Operation = Operations.RSHIFT(int c, a) }
    | _ -> failwith "Pattern not found"

let rec inputsWithValues (instructions : seq<Instruction>) = 
    let values = 
        instructions
        |> Seq.filter (fun f -> f.Value <> None)
        |> Seq.map (fun f -> f.Key, f.Value)
        |> Map.ofSeq
    
    let exist k = values.ContainsKey k
    let bothExist k h = exist k && exist h
    
    let getvalue k f = 
        if exist k then f values.[k].Value |> Some
        else None
    
    let binaryOP a b (f : uint16 -> uint16 -> uint16) = 
        if bothExist a b then (f values.[a].Value values.[b].Value) |> Some
        else None
    
    let shiftOP a b (f : uint16 -> int -> uint16) = 
        if exist a then f values.[a].Value b |> Some
        else None
    
    let negOP a = 
        if exist a then (~~~values.[a].Value) |> Some
        else None
    
    let result = 
        instructions 
        |> Seq.map (fun f -> 
               match f.Operation with
               | ASSIGNVALUE -> f
               | ASSIGN(a) -> 
                   { f with Value = 
                                if exist a then values.[a]
                                else None }
               | NOT(A) -> { f with Value = negOP A }
               | AND(A, B) -> { f with Value = binaryOP A B (&&&) }
               | INTAND(A, B) -> { f with Value = getvalue B ((&&&) A) }
               | INTOR(A, B) -> { f with Value = getvalue B ((|||) A) }
               | OR(A, B) -> { f with Value = binaryOP A B (|||) }
               | LSHIFT(A, B) -> { f with Value = shiftOP B A (<<<) }
               | RSHIFT(A, B) -> { f with Value = shiftOP B A (>>>) })
    
    if result |> Seq.exists (fun f -> f.Value = None) then 
        inputsWithValues result
    else result

let parsedValues = 
    "7.txt"
    |> filereadlines
    |> Seq.map (fun f -> parse f)

let a = 
    parsedValues
    |> inputsWithValues
    |> Seq.find (fun f -> f.Key = "a")

printfn "a's value is %d" a.Value.Value

let secondA = 
    parsedValues
    |> Set.ofSeq
    |> Set.remove (parsedValues |> Seq.find (fun f -> f.Key = "b"))
    |> Set.add ({ b with Value = a.Value })
    |> inputsWithValues
    |> Seq.find (fun f -> f.Key = "a")

printfn "Second a's value is %d" secondA.Value.Value
