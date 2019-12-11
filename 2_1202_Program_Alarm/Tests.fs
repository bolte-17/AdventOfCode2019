module Tests

open System
open Xunit
 
type RunningProgram = { head: int; program: int array } with
  member this.Next newState = { head = this.head + 4; program = newState }
  member this.Args = this.program |> Seq.skip this.head |> Seq.truncate 4 |> Seq.toList

type ProgramState = 
  | Halt of int array
  | Run of RunningProgram
  | Error of string

let parseProgram (programStr: string) = 
  programStr.Split(',') 
  |> Seq.map int
  |> Seq.toArray

let args (head: int, program: int array) = program 

let step state = 
  match state with
  | Run running -> 
    match running.Args with
    | 99 :: _ -> Halt running.program
    | op :: in1 :: in2 :: out -> 
      match op with 
      | 1 -> Some (+)
      | 2 -> Some (*)
      | _ -> None
      |> Option.fold (fun f -> f) running.program
      |> running.Next
      |> Run
    | _ -> Error "Unrecognized opcode"
  | _ -> Error ""

let exec (program: int array): 

  

[<Theory>]
[<InlineData("99", "99")>]
let ``My test`` (input: string, expected: string) =
  let program: ProgramState = { program = (parseProgram input); head = 0; }
  let expectedResult = parseProgram expected
  Assert.Equal<Collections.Generic.IEnumerable<int>>((execOne program).program, expectedResult)
