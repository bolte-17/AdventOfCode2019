module Program

open System.IO
open Tests

let totalFuel: seq<int> -> int = Seq.sumBy recFuel

[<EntryPoint>]
let main argv =
    File.ReadLines(argv.[0])
      |> Seq.map int
      |> totalFuel
      |> printfn "%d"
    0 // return an integer exit code
