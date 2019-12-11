module Program

open System
open System.IO
open Tests

[<EntryPoint>]
let main = function
  | [| inFile |] -> 
    let program = File.ReadAllText inFile |> parseProgram
    0
  | _ -> 1