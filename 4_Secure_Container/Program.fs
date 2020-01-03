module Day4.Program

open Day4.Tests

[<EntryPoint>]
let main =
    function
    | [| "test"; len; n |] ->
        countPasswordsAtLeast (int len) (int n) |> printfn "%d"
        0
    | [| len; min; max |] ->
        countPasswordsBetween (int len) (int min) (int max) |> printfn "%d"
        0
    | [| "testAll"; len |] ->
        testAllPasswords (int len)
        0

    | _ -> 1
