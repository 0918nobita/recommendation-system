module CosineSimilarity

type Vector = Vector of list<float>

let dotProduct (Vector a) (Vector b) : float =
    List.map2 ( * ) a b
    |> List.sum

let magnitude (Vector elems) : float =
    elems
    |> List.map (fun x -> x ** 2)
    |> List.sum
    |> sqrt

let cosineSimilarity (a : Vector) (b : Vector) : float =
    dotProduct a b / (magnitude a * magnitude b)
