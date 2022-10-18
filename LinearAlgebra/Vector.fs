module Vector

open DimensionMismatch
open Scalar

type Vector<'T>(elems: 'T[]) =
    let length = Array.length elems

    override _.ToString() =
        elems
        |> Array.map string
        |> String.concat ", "
        |> sprintf "[%s]"

    member _.Length = length

    member _.Elements = elems

    static member inline (+) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (+) a.Elements b.Elements
        |> Vector

    static member inline (-) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (-) a.Elements b.Elements
        |> Vector

    static member inline (*) (a: Vector< ^t>, Scalar b: Scalar< ^t>) : Vector< ^t> =
        a.Elements
        |> Array.map (fun a' -> a' * b)
        |> Vector

    static member inline (*) (Scalar a: Scalar< ^t>, b: Vector< ^t>) : Vector< ^t> =
        b.Elements
        |> Array.map ((*) a)
        |> Vector

let zeroVector<'a> = Vector Array.empty<'a>
