module Matrix

open DimensionMismatch
open Vector

exception InvalidShape

type Shape = Shape of rows: int * cols: int

type Mat<'T> =
    | Mat of shape: Shape * rows: array<Vec<'T>>

    override this.ToString() =
        match this with
        | Mat (_, rows) ->
            rows
            |> Array.map string
            |> String.concat ", "
            |> sprintf "[%s]"

    static member inline (+) (
        Mat (aShape, aRows): Mat< ^t>,
        Mat (bShape, bRows): Mat< ^t>
    ) : Mat< ^t> =
        if aShape <> bShape then raise DimensionMismatch

        let rows = Array.map2 (+) aRows bRows
        Mat (aShape, rows)

    static member inline (-) (
        Mat (aShape, aRows): Mat< ^t>,
        Mat (bShape, bRows): Mat< ^t>
    ) : Mat< ^t> =
        if aShape <> bShape then raise DimensionMismatch

        let rows = Array.map2 (-) aRows bRows
        Mat (aShape, rows)

    static member inline (*) (
        Mat (aShape, aRows): Mat< ^t>,
        b: ^t
    ) : Mat< ^t> =
        let rows = Array.map (fun aRow -> aRow * b) aRows
        Mat (aShape, rows)

    static member inline (*) (
        Mat (Shape(_, numACols), aRows): Mat< ^t>,
        b: Vec< ^t>
    ) : Vec< ^t> =
        if numACols <> Vec.length b then raise DimensionMismatch

        seq {
            for aRow in aRows -> aRow * b
        }
        |> Array.ofSeq
        |> Vec

    static member inline (*) (
        Mat (Shape (numARows, numACols), aRows): Mat< ^t>,
        Mat (Shape (numBRows, numBCols), bRows): Mat< ^t>
    ) : Mat< ^t> =
        if numACols <> numBRows then raise DimensionMismatch

        let bCols =
            seq {
                for i in 0 .. numBCols - 1 ->
                    Array.map (Vec.item i) bRows
            }
            |> Array.ofSeq
            |> Array.map Vec

        let rows =
            seq {
                for aRow in aRows ->
                    seq {
                        for bCol in bCols -> aRow * bCol
                    }
                    |> Array.ofSeq
                    |> Vec
            }
            |> Array.ofSeq

        Mat(Shape(numARows, numBCols), rows)

    static member inline (/) (
        Mat (aShape, aRows): Mat< ^t>,
        b: ^t
    ) : Mat< ^t> =
        let rows = Array.map (fun aRow -> aRow / b) aRows
        Mat (aShape, rows)

module Mat =
    let inline make (rows: array< ^t[]>) =
        if Array.isEmpty rows then raise InvalidShape
        let numCols = rows.[0].Length
        for row in rows do
            if row.Length <> numCols then raise InvalidShape
        let rows = Array.map Vec rows
        Mat (Shape (rows.Length, numCols), rows)
