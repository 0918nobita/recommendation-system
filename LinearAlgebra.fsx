type Scalar<'a> = Scalar of 'a

exception DimensionMismatch

type Vector<'T> =
    | Vec of list<'T>

    static member inline length (Vec elems) : int =
        List.length elems

    static member inline sameLength (a: Vector< ^t>) (b: Vector< ^t>) : bool =
        Vector.length< ^t> a = Vector.length< ^t> b

    static member inline (+) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if not (Vector.sameLength< ^t> a b) then raise DimensionMismatch

        let (Vec a) = a
        let (Vec b) = b

        List.map2 (+) a b
        |> Vec

    static member inline (-) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if not (Vector.sameLength< ^t> a b) then raise DimensionMismatch

        let (Vec a) = a
        let (Vec b) = b

        List.map2 (-) a b
        |> Vec

    static member inline (*) (
        Vec a: Vector< ^t>,
        Scalar b: Scalar< ^t>
    ) : Vector< ^t> =
        a
        |> List.map (fun a' -> a' * b)
        |> Vec

    static member inline (*) (
        Scalar a: Scalar< ^t>,
        Vec b: Vector< ^t>
    ) : Vector< ^t> =
        b
        |> List.map ((*) a)
        |> Vec

type Shape = Shape of rows: int * cols: int

module Shape =
    let inline rows (Shape (rows, _)) = rows
    let inline cols (Shape (_, cols)) = cols

exception InvalidShape

type Matrix<'T>(rows: list<list<'T>>) =
    let numRows = List.length rows
    let numCols = List.length (List.head rows)

    do for row in rows do
        if List.length row <> numCols then raise InvalidShape

    override _.ToString() =
        sprintf "%A" rows

    member _.Shape = Shape (numRows, numCols)

    member _.Rows = rows

    member _.Columns =
        seq {
            for i in 0 .. numCols - 1 ->
                List.map (List.item i) rows
        }

    static member inline (+) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if a.Shape <> b.Shape then raise DimensionMismatch

        let a = a.Rows
        let b = b.Rows

        List.map2 (List.map2 (+)) a b
        |> Matrix

    static member inline (-) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if a.Shape <> b.Shape then raise DimensionMismatch

        let a = a.Rows
        let b = b.Rows

        List.map2 (List.map2 (-)) a b
        |> Matrix

    static member inline (*) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        let aCols = Shape.cols a.Shape
        let bRows = Shape.rows b.Shape
        if aCols <> bRows then raise DimensionMismatch

        seq {
            for aRow in a.Rows ->
                List.ofSeq <| seq {
                    for bCol in b.Columns ->
                        List.sum <| List.map2 (*) aRow bCol
                }
        }
        |> List.ofSeq
        |> Matrix

(Matrix [[1; 3]; [-3; 4]])
* (Matrix [[1; 9]; [9; 3]])
|> printfn "%O" // [[28; 18]; [33; -15]]

(Matrix [[1; 0]; [4; 1]; [-1; 2]])
* (Matrix [[1; 3]; [2; -1]])
|> printfn "%O" // [[1; 3]; [6; 11]; [3; -5]]

try
    Matrix [[1; 2]; [3; 4; 5]]
    |> printfn "%O (this should not be printed)"
with
| InvalidShape -> eprintfn "Invalid shape"

try
    Matrix [[1; 0]; [4; 1]; [-1; 2]]
    * Matrix [[1; 3]; [2; -1]; [0; 0]]
    |> printfn "%O"
with
| DimensionMismatch -> eprintfn "Dimension mismatch"
