module Program

open Fraction
open Scalar
open Vector

let vec = Vec [| Frac.ofInt 5; Frac.ofInt 2 |]
printfn "%O" vec
printfn "%O" <| vec / (Scalar (Frac.ofInt 3))
