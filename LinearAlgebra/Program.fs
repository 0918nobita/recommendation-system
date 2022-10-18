module Program

open Fraction
open Matrix
open Scalar
open Vector

printfn "%O" <| Fraction(2, 3) + Fraction(3, 2) // 13/6
printfn "%O" <| Fraction(4, 6) - Fraction(3, 2) // -5/6
printfn "%O" <| Fraction(2, 3) * Fraction(3, 2) // 1
printfn "%O" <| Fraction(2, 3) / Fraction(3, 2) // 4/9

Matrix [|[|1; 3|]; [|-3; 4|]|]
* Matrix [|[|1; 9|]; [|9; 3|]|]
|> printfn "%O" // [[28, 18], [33, -15]]

Matrix [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|]
* Matrix [|[|1; 3|]; [|2; -1|]|]
|> printfn "%O" // [[1, 3], [6, 11], [3, -5]]

Matrix [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|] * Scalar 2
|> printfn "%O" // [[2, 0], [8, 2], [-2, 4]]

Scalar 4 * Matrix [|[|3; 0|]; [|1; 2|]|]
|> printfn "%O" // [[12, 0], [4, 8]]

Matrix [|[|1; 2|]; [|0; 3|]|] * Vector [|2; 1|]
|> printfn "%A" // [4, 3]
