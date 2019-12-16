module Day3.Tests

open System
open Xunit

let (|Direction|_|) = function 
  | (s: string) when s.Length > 0 -> 
    match s.[0] with
    | 'R' -> Some ( 1,  0)
    | 'L' -> Some (-1,  0)
    | 'U' -> Some ( 0,  1)
    | 'D' -> Some ( 0, -1)
    | _ -> None
  | _ -> None

let (|Magnitude|_|) (s: string) = try int s.[1..] |> Some with _ -> None

let parseVec = function
  | Direction d & Magnitude m -> (d, m)
  | _ -> raise <| Exception "Invalid vector"

[<Theory>]
[<InlineData("R8", 1, 0, 8)>]
[<InlineData("U5", 0, 1, 5)>]
[<InlineData("L5", -1, 0, 5)>]
[<InlineData("D32", 0, -1, 32)>]
let ``parses moves correctly`` (testValue: string, xVec: int, yVec: int, mag: int) =
  Assert.Equal(((xVec, yVec), mag), parseVec testValue)

let points origin ((dx, dy), m) = [0..m] |> List.scan (fun (x,y) _ -> (x + dx, y + dy)) origin

let add2 (x, y) (dx, dy) = (x + dx, y + dy)

let wirePath (wireStr: string) = 
  wireStr.Split(',')
  |> Seq.map parseVec
  |> Seq.collect (fun (d, m) -> Seq.replicate m d)
  |> Seq.scan add2 (0, 0)
  |> set

[<Theory>]
[<InlineData("R8,U5,L5,D2", 0, 0, true)>]
[<InlineData("R8,U5,L5,D2", 8, 0, true)>]
[<InlineData("R8,U5,L5,D2", 8, 1, true)>]
[<InlineData("R8,U5,L5,D2", 8, -1, false)>]
[<InlineData("R8,U5,L5,D2", 3, 5, true)>]
[<InlineData("R8,U5,L5,D2", 2, 5, false)>]
[<InlineData("R8,U5,L5,D2", 3, 3, true)>]
[<InlineData("R8,U5,L5,D2", 3, 2, false)>]
let ``finds points on described path`` (testPath: string, x, y, isOnPath) =
  if isOnPath then 
    Assert.Contains ((x, y), (wirePath testPath))
  else 
    Assert.DoesNotContain ((x, y), (wirePath testPath))

let norm (x, y) = abs x + abs y

let wireIntersections paths =
  paths
  |> Seq.map wirePath
  |> Set.intersectMany
  |> Set.remove (0,0)

let closestIntersection paths =
  paths
  |> wireIntersections
  |> Set.map norm
  |> Seq.min

[<Theory>]
[<InlineData("R8,U5,L5,D2", "U7,R6,D4,L4", 6)>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)>]
let ``finds distance to closest intersection`` (path1, path2, expected) =
  Assert.Equal(expected, closestIntersection [path1; path2])
