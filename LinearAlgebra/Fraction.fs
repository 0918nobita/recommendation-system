module Fraction

let rec gcd x y =
    if y = 0 then x else gcd y (x % y)

type Frac(numerator: int, denominator: int) =
    let numerator = if denominator < 0 then -numerator else numerator
    let denominator = abs denominator

    let gcd' = gcd numerator denominator
    let numerator = numerator / gcd'
    let denominator = denominator / gcd'

    override _.ToString() =
        if denominator = 1 then
            string numerator
        else
            sprintf "%d/%d" numerator denominator

    member _.Numerator = numerator

    member _.Denominator = denominator

    static member inline Zero = Frac(0, 1)

    static member inline (+) (a: Frac, b: Frac) =
        let denoGcd = gcd a.Denominator b.Denominator
        let aMul = b.Denominator / denoGcd
        let bMul = a.Denominator / denoGcd
        Frac(a.Numerator * aMul + b.Numerator * bMul, a.Denominator * aMul)

    static member inline (-) (a: Frac, b: Frac) =
        let denoGcd = gcd a.Denominator b.Denominator
        let aMul = b.Denominator / denoGcd
        let bMul = a.Denominator / denoGcd
        Frac(a.Numerator * aMul - b.Numerator * bMul, a.Denominator * aMul)

    static member inline (*) (a: Frac, b: Frac) =
        Frac(a.Numerator * b.Numerator, a.Denominator * b.Denominator)

    static member inline (/) (a: Frac, b: Frac) =
        Frac(a.Numerator * b.Denominator, a.Denominator * b.Numerator)

module Frac =
    let inline ofInt x = Frac(x, 1)
