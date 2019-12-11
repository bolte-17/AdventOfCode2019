module Day2.Program

open System
open System.IO
open Tests

let setParams noun verb program = 
  program
  |> Array.toList
  |> function head :: _ :: _ :: rest -> head :: noun :: verb :: rest | p -> p

[<EntryPoint>]
let main = function
  | [| inFile |] -> 
    File.ReadAllText inFile 
    |> parseProgram 
    |> setParams 12 2
    |> execute 
    |> function 
      | Ok a -> printfn "%i" (Array.head a)
      | Error e -> printfn "Error: %s" e
    0
  | [| inFile; target |] -> 
    let program = File.ReadAllText inFile |> parseProgram
    Seq.allPairs [0..99] [0..99] 
    |> Seq.find (
      fun (i, j) -> 
        program 
        |> setParams i j 
        |> execute 
        |> function 
          | Ok result -> Seq.head result = int target 
          | _ -> false
    )
    |> fun (noun, verb) -> printf "100 * %i + %i = %i" noun verb (100 * noun + verb)
    0
  | _ -> 1