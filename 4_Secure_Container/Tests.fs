module Day4.Tests

open Xunit

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
            let res = f x
            cache := (!cache).Add(x, res)
            res

#nowarn "40"
let rec choose =
    memoize (function
        | (_, 0) -> 1
        | (n, k) when k > n -> 0
        | (n, k) when k > n / 2 -> choose (n, n - k)
        | (n, k) -> n * choose (n - 1, k - 1) / k)

[<Theory>]
[<InlineData(5, 3, 10)>]
let testChoose (n, k, expected) = Assert.Equal(expected, choose (n, k))

let multichoose (n, k) = choose (n + k - 1, k)

[<Theory>]
[<InlineData(5, 3, 35)>]
[<InlineData(1, 3, 1)>]
let testMultichoose (n, k, expected) = Assert.Equal(expected, multichoose (n, k))

let rec nondecreasings minDigit mins = 
    match (minDigit, mins) with
    | (_, []) -> 1
    | (m, _) when m > 9 -> 0
    | (m, n :: _) when m > n -> multichoose (10 - m, mins.Length)
    | (_, [n]) -> 10 - n
    | (_, n :: rest) -> multichoose (10 - (n + 1), mins.Length) + nondecreasings n rest

let strictlyIncreasings length minDigit =
    choose (10 - minDigit, length)

let rec passwordsAtLeast = function
    | (next :: _, _) when next > 9 -> 0
    | ([], (_, 2)) -> 1
    | (next :: _ as mins, (m, 2)) when next > m -> nondecreasings next mins 
    | (next :: _ as mins, (m, n)) when m > next -> passwordsAtLeast (List.replicate mins.Length m, (m,n))
    | ((next :: rest), (prev, n)) ->
          passwordsAtLeast ((next + 1) :: (List.replicate rest.Length 0), (prev, n))
          + passwordsAtLeast (rest, (next, if prev = next then n + 1 else 1))
    | _ -> 0

let digitSeq =
    string
    >> Seq.map (int >> ((-) (int '0') >> (~-)))
    >> Seq.toList

let padLeft len m =
    let list = digitSeq m
    list |> List.append (List.replicate (max (len - list.Length) 0) 0)

let normalizeMin len m =
    let num = padLeft len m
    num
    |> List.scan (fun ((m, decreased) as s) n -> if decreased then s else (max m n, m > n)) (0, false)
    |> List.map fst
    |> List.tail
    
[<Theory>]
[<InlineData(6, 240920, 244444)>]
[<InlineData(6, 789857, 789999)>]
let testNormalizeMin (len, d, expected) = 
  Assert.Equal<int>(digitSeq expected, normalizeMin len d)


let isValidPassword len n =
    let digits = padLeft len n
    not (digits 
    |> Seq.pairwise 
    |> Seq.exists ((<||) (>)))
    && 
    digits
    |> List.groupBy id
    |> Seq.exists (snd >> List.length >> ((=) 2))

let countPasswordsAtLeast len n =
  passwordsAtLeast (normalizeMin len n, (-1, 0))

let countPasswordsBetween len lower upper =
    let lowerCount = countPasswordsAtLeast len lower
    let upperCount = countPasswordsAtLeast len upper
    lowerCount - upperCount + if isValidPassword len upper then 1 else 0

[<Theory>]
[<InlineData(2, 0, 99, 10)>]
[<InlineData(2, 0, 98, 9)>]
[<InlineData(3, 011, 099, 9)>]
[<InlineData(3, 000, 010, 9)>] // 001 - 009, not 000
[<InlineData(3, 000, 099, 18)>]
[<InlineData(3, 000, 111, 18)>]
[<InlineData(3, 000, 112, 19)>]
[<InlineData(5, 00000, 00023, 2)>]
[<InlineData(6, 888899, 999999, 2)>] // 889999, 888899
[<InlineData(6, 777777, 888898, 11)>] 
[<InlineData(6, 240920, 789857, 750)>] 
let testCountPasswordsBetween (len, lower, upper, expected) =
    Assert.Equal(expected, countPasswordsBetween len lower upper)

let testAllPasswords len =
  let mismatches = 
    [(int (10.**float len))-1..-1..0]
    |> Seq.map (fun n -> (n, (countPasswordsAtLeast len n)))
    |> Seq.pairwise
    |> Seq.filter (fun ((_, n), (j,m)) -> 
      if isValidPassword len j then (n + 1) <> m else n <> m 
    )
  for ((_,n), (j,m)) in mismatches do printfn "%d -> %d" j (m - n)
  Assert.Empty(mismatches)

let countPasswordsBetweenBrute len lower upper = 
  [lower..upper] 
    |> Seq.filter (isValidPassword len) 
    |> Seq.map (fun f -> printfn "%d" f; f)
    |> Seq.length