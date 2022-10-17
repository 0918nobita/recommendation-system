type Scalar<'a> = Scalar of 'a

type Vector<'T> =
    | Vec of list<'T>

    static member inline length (Vec elems) : int =
        List.length elems

    static member inline sameLength (a: Vector< ^t>) (b: Vector< ^t>) : bool =
        Vector.length< ^t> a = Vector.length< ^t> b

    static member inline (+) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if not (Vector.sameLength< ^t> a b) then failwith "Dimension mismatch"

        let (Vec a) = a
        let (Vec b) = b

        List.map2 (+) a b
        |> Vec

    static member inline (-) (a: Vector< ^t>, b: Vector< ^t>) : Vector< ^t> =
        if not (Vector.sameLength< ^t> a b) then failwith "Dimension mismatch"

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

type Matrix<'T> =
    | Matrix of numRows: int * numCols: int * rows: list<list<'T>>

    override this.ToString() =
        match this with
        | Matrix (_, _, rows) -> sprintf "%A" rows

    static member inline rows (Matrix (_, _, rows): Matrix< ^t>) : list<list< ^t>> =
        rows

    static member inline columns (Matrix (_, numCols, rows) : Matrix< ^t>) : list<list< ^t>> =
        seq {
            for i in 0 .. numCols - 1 ->
                List.map (List.item i) rows
        }
        |> List.ofSeq

    static member inline numRows (Matrix (numRows, _, _)) : int =
        numRows

    static member inline numCols (Matrix (_, numCols, _)) : int =
        numCols

    static member inline shape (mat: Matrix< ^t>) : int * int =
        Matrix.numRows< ^t> mat, Matrix.numCols< ^t> mat

    static member inline sameShape (a: Matrix< ^t>) (b: Matrix< ^t>) : bool =
        Matrix.shape< ^t> a = Matrix.shape< ^t> b

    static member inline (+) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if not (Matrix.sameShape<'t> a b) then failwith "Dimension mismatch"

        let (numRows, numCols) = Matrix.shape< ^t> a

        let a = Matrix.rows< ^t> a
        let b = Matrix.rows< ^t> b

        let rows = List.map2 (List.map2 (+)) a b
        Matrix (numRows, numCols, rows)

    static member inline (-) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if not (Matrix.sameShape< ^t> a b) then failwith "Dimension mismatch"

        let (numRows, numCols) = Matrix.shape< ^t> a

        let a = Matrix.rows< ^t> a
        let b = Matrix.rows< ^t> b

        let rows = List.map2 (List.map2 (-)) a b
        Matrix (numRows, numCols, rows)

    static member inline (*) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        let aCols = Matrix.numCols< ^t> a
        let bRows = Matrix.numRows< ^t> b
        if aCols <> bRows then failwith "Dimension mismatch"

        let rows =
            seq {
                for aRow in Matrix.rows< ^t> a ->
                    List.ofSeq <| seq {
                        for bCol in Matrix.columns< ^t> b ->
                            List.sum <| List.map2 (*) aRow bCol
                    }
            }
            |> List.ofSeq
        
        Matrix (Matrix.numRows< ^t> a, Matrix.numCols< ^t> b, rows)

module Matrix =
    let inline make(rows: list<list<'t>>) : Matrix<'t> =
        let numRows = List.length rows
        let numCols = List.length (List.head rows)
        Matrix (numRows, numCols, rows)

(Matrix.make [[1; 3]; [-3; 4]])
* (Matrix.make [[1; 9]; [9; 3]])
|> printfn "%O" // [[28; 18]; [33; -15]]

(Matrix.make [[1; 0]; [4; 1]; [-1; 2]])
* (Matrix.make [[1; 3]; [2; -1]])
|> printfn "%O" // [[1; 3]; [6; 11]; [3; -5]]

let a = Matrix.make [[1; 0]; [4; 1]; [-1; 2]]
let b = Matrix.make [[1; 3]; [2; -1]; [0; 0]]
try
    printfn "%O" <| a * b
with
    _ -> eprintfn "Dimension mismatch: %O * %O" a b
