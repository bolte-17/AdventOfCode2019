module Day3.Program
open System.IO
open Day3.Tests

let [<EntryPoint>] main = function
  | [| inFile |] -> 
    File.ReadAllLines inFile
    |> closestIntersection
    |> printfn "%d" 
    0
  | _ -> 1
