type Scalar<'a> = Scalar of 'a

exception InvalidShape

exception DimensionMismatch

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

type Shape = Shape of rows: int * cols: int

module Shape =
    let inline rows (Shape (rows, _)) = rows
    let inline cols (Shape (_, cols)) = cols

type Matrix<'T>(rows: array<'T[]>) =
    let numRows = Array.length rows

    do if Array.isEmpty rows then raise InvalidShape
    let numCols = Array.length (rows.[0])

    do for row in rows do
        if Array.length row <> numCols then raise InvalidShape

    override _.ToString() =
        rows
        |> Array.map (Array.map string >> String.concat ", " >> sprintf "[%s]")
        |> String.concat ", "
        |> sprintf "[%s]"

    member _.Shape = Shape (numRows, numCols)

    member _.Rows = rows

    member _.Columns =
        seq {
            for i in 0 .. numCols - 1 ->
                Array.map (Array.item i) rows
        }

    static member inline (+) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if a.Shape <> b.Shape then raise DimensionMismatch

        Array.map2 (Array.map2 (+)) a.Rows b.Rows
        |> Matrix

    static member inline (-) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if a.Shape <> b.Shape then raise DimensionMismatch

        Array.map2 (Array.map2 (-)) a.Rows b.Rows
        |> Matrix

    static member inline (*) (Scalar a: Scalar< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        Array.map (Array.map ((*) a)) b.Rows
        |> Matrix

    static member inline (*) (a: Matrix< ^t>, Scalar b: Scalar< ^t>) : Matrix< ^t> =
        Array.map (Array.map (fun a' -> a' * b)) a.Rows
        |> Matrix

    static member inline (*) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        let aCols = Shape.cols a.Shape
        let bRows = Shape.rows b.Shape
        if aCols <> bRows then raise DimensionMismatch

        seq {
            for aRow in a.Rows ->
                Array.ofSeq <| seq {
                    for bCol in b.Columns ->
                        Array.sum <| Array.map2 (*) aRow bCol
                }
        }
        |> Array.ofSeq
        |> Matrix

    static member inline (*) (a: Matrix< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if Shape.cols a.Shape <> b.Length then raise DimensionMismatch

        seq {
            for aRow in a.Rows ->
                Array.sum <| Array.map2 (*) aRow b.Elements
        }
        |> Array.ofSeq
        |> Vector

Matrix [|[|1; 3|]; [|-3; 4|]|]
* Matrix [|[|1; 9|]; [|9; 3|]|]
|> printfn "%O" // [[28, 18], [33, -15]]

Matrix [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|]
* Matrix [|[|1; 3|]; [|2; -1|]|]
|> printfn "%O" // [[1, 3], [6, 11], [3, -5]]

try
    Matrix [|[|1; 2|]; [|3; 4; 5|]|]
    |> printfn "%O (this should not be printed)"
with
| InvalidShape -> eprintfn "Invalid shape"

try
    Matrix [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|]
    * Matrix [|[|1; 3|]; [|2; -1|]; [|0; 0|]|]
    |> printfn "%O (this should not be printed)"
with
| DimensionMismatch -> eprintfn "Dimension mismatch"

Matrix [|[|1; 0|]; [|4; 1|]; [|-1; 2|]|] * Scalar 2
|> printfn "%O" // [[2, 0], [8, 2], [-2, 4]]

Scalar 4 * Matrix [|[|3; 0|]; [|1; 2|]|]
|> printfn "%O" // [[12, 0], [4, 8]]

Matrix [|[|1; 2|]; [|0; 3|]|] * Vector [|2; 1|]
|> printfn "%A" // [4, 3]
