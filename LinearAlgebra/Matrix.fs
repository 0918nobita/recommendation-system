module Matrix

open DimensionMismatch
open Scalar
open Vector

exception InvalidShape

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