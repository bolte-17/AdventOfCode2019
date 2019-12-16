module Day6.Program
open System.IO
open Tests

let [<EntryPoint>] main = 
  function
  | [| inFile |] -> 
    File.ReadAllLines inFile
    |> parseOrbits
    |> countAllOrbits
    |> printfn "%i"
    0
  | [| inFile; src; dest |] -> 
    File.ReadAllLines inFile
    |> parseOrbits
    |> distance <| (src, dest)
    |> printfn "%i"
    0
  | _ -> 1
