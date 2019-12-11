module Day2.Tests

open System
open Xunit

let (|Op3|_|) = function
  | 1 -> Some (+)
  | 2 -> Some (*)
  | _ -> None

let (|HaltOp|_|) inst = match Array.head inst with 99 -> Some () | _ -> None 

let (|Src|) i (program: int array) = program.[i]; 
let (|Dest|) i program value = 
  Array.set program i value
  program

type OpResult = 
  | Step of (int array -> int array)
  | RaiseError of string
  | Halt

let parseInst = function
  | [| Op3 op; Src src1; Src src2; Dest dest |] -> 
    Step (fun program -> 
      dest program <| op (src1 program) (src2 program)
    )
  | HaltOp -> Halt
  | _ -> RaiseError "Unrecognized Operation"

let rec exec (program: int array) instPtr = 
  program.[instPtr..instPtr + 3 |> min <| program.Length - 1] 
  |> parseInst
  |> function
  | Step f -> exec (f program) (instPtr + 4) 
  | Halt -> Ok program
  | RaiseError err -> Error err

let execute program = exec (Seq.toArray program) 0

let parseProgram (str: string) = str.Split ',' |> Array.map int

[<Theory>]
[<InlineData("99", "99")>]
let ``Halts on 99`` (input: string, expected: string) =
  Assert.Equal(Ok (parseProgram expected), parseProgram input |> execute)
