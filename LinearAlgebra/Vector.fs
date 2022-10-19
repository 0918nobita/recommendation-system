module Vector

open DimensionMismatch

type Vec<'T> =
    | Vec of 'T[]

    override this.ToString() =
        match this with
        | Vec elems ->
            elems
            |> Array.map string
            |> String.concat ", "
            |> sprintf "[%s]"

    static member inline Zero: Vec<'T> = Vec Array.empty

    static member inline (+) (Vec a: Vec< ^t>, Vec b: Vec< ^t>) : Vec< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (+) a b
        |> Vec

    static member inline (-) (Vec a: Vec< ^t>, Vec b: Vec< ^t>) : Vec< ^t> =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (-) a b
        |> Vec

    static member inline (*) (Vec a: Vec< ^t>, b: ^t) : Vec< ^t> =
        a
        |> Array.map (fun a' -> a' * b)
        |> Vec

    static member inline (*) (Vec a: Vec< ^t>, Vec b: Vec< ^t>) : ^t =
        if a.Length <> b.Length then raise DimensionMismatch

        Array.map2 (*) a b
        |> Array.sum

    static member inline (/) (Vec a: Vec< ^t>, b: ^t) : Vec< ^t> =
        a
        |> Array.map (fun a' -> a' / b)
        |> Vec

module Vec =
    let inline length< ^t> (Vec a: Vec< ^t>) : int = Array.length a

    let inline item (i: int) (Vec elems: Vec< ^t>) : ^t = elems.[i]
    
    let inline sum (Vec elems: Vec< ^t>): ^t = Array.sum elems

    let inline map (f: ^a -> ^b) (Vec elems: Vec< ^a>) : Vec< ^b> =
        elems
        |> Array.map f
        |> Vec
