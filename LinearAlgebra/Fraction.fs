module Fraction

let rec gcd x y =
    if y = 0 then x else gcd y (x % y)

type Fraction(numerator: int, denominator: int) =
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

    static member inline (+) (a: Fraction, b: Fraction) =
        let denoGcd = gcd a.Denominator b.Denominator
        let aMul = b.Denominator / denoGcd
        let bMul = a.Denominator / denoGcd
        Fraction(a.Numerator * aMul + b.Numerator * bMul, a.Denominator * aMul)

    static member inline (-) (a: Fraction, b: Fraction) =
        let denoGcd = gcd a.Denominator b.Denominator
        let aMul = b.Denominator / denoGcd
        let bMul = a.Denominator / denoGcd
        Fraction(a.Numerator * aMul - b.Numerator * bMul, a.Denominator * aMul)

    static member inline (*) (a: Fraction, b: Fraction) =
        Fraction(a.Numerator * b.Numerator, a.Denominator * b.Denominator)

    static member inline (/) (a: Fraction, b: Fraction) =
        Fraction(a.Numerator * b.Denominator, a.Denominator * b.Numerator)
