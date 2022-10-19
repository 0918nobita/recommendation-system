module Program

open Fraction
open Matrix
open Vector

Mat.make [|[|1; 3|]; [|-3; 4|]|]
* Mat.make [|[|1; 9|]; [|9; 3|]|]
|> printfn "%O" // [[28, 18], [33, -15]]

Mat.make [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|]
* Mat.make [|[|1; 3|]; [|2; -1|]|]
|> printfn "%O" // [[1, 3], [6, 11], [3, -5]]

Mat.make [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|] * 2
|> printfn "%O" // [[2, 0], [8, 2], [-2, 4]]

Mat.make [|[|1; 2|]; [|0; 3|]|] * Vec [|2; 1|]
|> printfn "%O" // [4, 3]

printfn "%O" <| Frac(2, 3) + Frac(3, 2) // 13/6
printfn "%O" <| Frac(4, 6) - Frac(3, 2) // -5/6
printfn "%O" <| Frac(2, 3) * Frac(3, 2) // 1
printfn "%O" <| Frac(2, 3) / Frac(3, 2) // 4/9

let vec = Vec [| Frac.ofInt 5; Frac.ofInt 2 |]

vec
|> printfn "%O" // [5, 2]

vec / (Frac.ofInt 3)
|> printfn "%O" // [5/3, 2/3]
