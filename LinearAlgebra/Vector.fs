module Vector

open DimensionMismatch
open Scalar

type Vec<'T>(elems: 'T[]) =
    let length = Array.length elems

    override _.ToString() =
        elems
        |> Array.map string
        |> String.concat ", "
        |> sprintf "[%s]"

    member _.Length = length

    member _.Elements = elems

    static member inline (+) (a: Vec< ^t>, b: Vec< ^t>) : Vec< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (+) a.Elements b.Elements
        |> Vec

    static member inline (-) (a: Vec< ^t>, b: Vec< ^t>) : Vec< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (-) a.Elements b.Elements
        |> Vec

    static member inline (*) (a: Vec< ^t>, Scalar b: Scalar< ^t>) : Vec< ^t> =
        a.Elements
        |> Array.map (fun a' -> a' * b)
        |> Vec

    static member inline (*) (Scalar a: Scalar< ^t>, b: Vec< ^t>) : Vec< ^t> =
        b.Elements
        |> Array.map ((*) a)
        |> Vec

    static member inline (/) (a: Vec< ^t>, Scalar b: Scalar< ^t>) : Vec< ^t> =
        a.Elements
        |> Array.map (fun a' -> a' / b)
        |> Vec

    static member inline (/) (Scalar a: Scalar< ^t>, b: Vec< ^t>) : Vec< ^t> =
        b.Elements
        |> Array.map ((/) a)
        |> Vec

let zeroVec<'a> = Vec Array.empty<'a>
