module Day6.Tests

open Xunit

let parseOrbits (input: string seq) =
    input
    |> Seq.choose
        ((fun s -> s.Split(')', 2))
         >> function
         | [| p; m |] -> Some(m, p)
         | _ -> None)
    |> Map.ofSeq

let rec depth (orbits: Map<string, string>) =
    function
    | "COM" -> 0
    | moon -> 1 + (depth orbits orbits.[moon])

let countAllOrbits orbits = orbits |> Seq.sumBy ((depth orbits) << (fun p -> p.Key))

let sampleOrbits = [ "COM)B"; "B)C"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L" ]

[<Theory>]
[<InlineData("COM", 0)>]
[<InlineData("B", 1)>]
[<InlineData("C", 2)>]
[<InlineData("D", 3)>]
[<InlineData("E", 4)>]
[<InlineData("F", 5)>]
[<InlineData("G", 2)>]
[<InlineData("H", 3)>]
[<InlineData("I", 4)>]
[<InlineData("J", 5)>]
[<InlineData("K", 6)>]
[<InlineData("L", 7)>]
let testOrbitCount (moon, expected) =
    Assert.Equal(expected, depth <| parseOrbits sampleOrbits <| moon)

[<Fact>]
let testCountAllOrbits () =
  Assert.Equal(42, countAllOrbits <| parseOrbits sampleOrbits)

let rec distance orbits = function 
  | (x, y) when x = y -> 0
  | (x, y) -> 1 + distance orbits (if depth orbits x > depth orbits y then (orbits.[x], y) else (x, orbits.[y]))

let sampleDistOrbits = Seq.concat [sampleOrbits; ["K)YOU"; "I)SAN"]]

[<Theory>]
[<InlineData("YOU", "SAN", 6)>]
[<InlineData("I", "K", 4)>]
let testDistance (src, dest, expected) =
  let orbits = (parseOrbits sampleDistOrbits)
  Assert.Equal(expected, distance orbits (src, dest))

