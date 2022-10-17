type Scalar<'a> = Scalar of 'a

type Vector<'T when 'T : (static member (+) : 'T * 'T -> 'T)
                and 'T : (static member (-) : 'T * 'T -> 'T)
                and 'T : (static member (*) : 'T * 'T -> 'T)> =
    | Vec of list<'T>

    static member inline (+) (
        Vec a: Vector< ^t>,
        Vec b: Vector< ^t>
    ) : Vector< ^t> =
        List.map2 (+) a b
        |> Vec

    static member inline (-) (
        Vec a: Vector< ^t>,
        Vec b: Vector< ^t>
    ) : Vector< ^t> =
        List.map2 (-) a b
        |> Vec

    static member inline (*) (
        Vec a: Vector< ^t>,
        Scalar b: Scalar< ^t>
    ) : Vector< ^t> =
        a
        |> List.map (fun x -> x * b)
        |> Vec

    static member inline (*) (
        Scalar a: Scalar< ^t>,
        Vec b: Vector< ^t>
    ) : Vector< ^t> =
        b
        |> List.map ((*) a)
        |> Vec

printfn "%A" <| Vec [1; 2; 3] + Vec [4; 5; 6]       // [5; 7; 9]
printfn "%A" <| Vec [1.; 2.; 3.] + Vec [4; 5; 6]    // [5.0; 7.0; 9.0]
// printfn "%A" <| Vec [1; 2; 3] + Vec [4.; 5.; 6.] // type mismatch

printfn "%A" <| Vec [6; 5; 4] - Vec [1; 2; 3]       // [5; 3; 1]
printfn "%A" <| Vec [6.; 5.; 4.] - Vec [1; 2; 3]    // [5.0; 3.0; 1.0]
// printfn "%A" <| Vec [6; 5; 4] - Vec [1.; 2.; 3.] // type mismatch

printfn "%A" <| (Scalar 3) * Vec [1; 2; 3]       // [3; 6; 9]
printfn "%A" <| (Scalar 3.) * Vec [1; 2; 3]      // [3.0; 6.0; 9.0]
// printfn "%A" <| (Scalar 3) * Vec [1.; 2.; 3.] // type mismatch

printfn "%A" <| (Vec [1; 2; 3]) * (Scalar 4)     // [4; 8; 12]
printfn "%A" <| (Vec [1.; 2.; 3.]) * (Scalar 4)  // [4.0; 8.0; 12.0]
// printfn "%A" <| (Vec [1; 2; 3]) * (Scalar 4.) // type mismatch
