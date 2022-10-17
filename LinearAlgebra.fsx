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
    | Matrix of list<list<'T>>

    static member inline numRows< ^t> (mat: Matrix< ^t>) : int =
        match mat with
        | Matrix rows -> List.length rows

    static member inline numCols< ^t> (mat: Matrix< ^t>) : int =
        match mat with
        | Matrix rows -> List.length (List.head rows)

    static member inline shape< ^t> (mat: Matrix< ^t>) : int * int =
        Matrix.numRows< ^t> mat, Matrix.numCols< ^t> mat

    static member inline sameShape< ^t> (matA: Matrix< ^t>) (matB: Matrix< ^t>) : bool =
        Matrix.shape< ^t> matA = Matrix.shape< ^t> matB

    static member inline (+) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if not (Matrix.sameShape< ^t> a b) then failwith "Dimension mismatch"

        let (Matrix a) = a
        let (Matrix b) = b

        List.map2 (List.map2 (+)) a b
        |> Matrix

    static member inline (-) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        if not (Matrix.sameShape< ^t> a b) then failwith "Dimension mismatch"

        let (Matrix a) = a
        let (Matrix b) = b

        List.map2 (List.map2 (-)) a b
        |> Matrix

    static member inline (*) (a: Matrix< ^t>, b: Matrix< ^t>) : Matrix< ^t> =
        let aRows = Matrix.numRows< ^t> a
        let bCols = Matrix.numCols< ^t> b
        if aRows <> bCols then failwith "Dimension mismatch"

        let (Matrix a) = a
        let (Matrix b) = b

        seq {
            for aRow in a do
                yield List.ofSeq <| seq {
                    for iCol in 0 .. (List.length (List.head b) - 1) do
                        let bCol = b |> List.map (fun bRow -> bRow.[iCol])
                        yield List.sum <| List.map2 (*) aRow bCol
                }
        }
        |> List.ofSeq
        |> Matrix

printfn "%A" <| Matrix [[1; 3]; [-3; 4]] * Matrix [[1; 9]; [9; 3]] // [[28; 18]; [33; -15]]
