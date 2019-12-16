module Day1.Tests

open System
open Xunit

let fuel mass = mass / 3 - 2;

let rec recFuel mass = 
  match fuel(mass) with 
    | f when f > 0 -> f + recFuel(f)
    | _ -> 0

[<Theory>]
[<InlineData(12,2)>]
[<InlineData(14,2)>]
[<InlineData(1969, 654)>]
[<InlineData(100756, 33583)>]
let ``Calcluates fuel requirements for given mass`` (input: int, expected: int) = 
  Assert.Equal(expected, fuel(input))

[<Theory>]
[<InlineData(14,2)>]
[<InlineData(1969,966)>]
[<InlineData(100756, 50346)>]
let ``Calcluates fuel requirements including fuel`` (input: int, expected: int) = 
  Assert.Equal(expected, recFuel(input))
