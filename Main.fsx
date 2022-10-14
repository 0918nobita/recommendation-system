let dotProduct (a : list<int>) (b : list<int>) : int =
    List.map2 ( * ) a b
    |> List.sum

let magnitude (rs : list<int>) : float =
    rs
    |> List.map (fun r -> float r ** 2)
    |> List.sum
    |> sqrt

let cosineSimilarity (a : list<int>) (b : list<int>) : float =
    let magnitudeA = magnitude a
    let magnitudeB = magnitude b
    if magnitudeA = 0. || magnitudeB = 0.
    then -1.
    else float (dotProduct a b) / (magnitudeA * magnitudeB)

let predictRate (user : int) (item : int) (ratings: list<list<int>>) : float =
    ratings
    |> List.mapi (fun userIdx items -> (userIdx, items))
    |> List.choose (fun (userIdx, items) ->
        if userIdx = user
        then None
        else
            Some (cosineSimilarity ratings.[user] items * (float items.[item])))
    |> List.sum

let a = [1; 2; 5; 0]
let b = [2; 0; 4; 3]
let c = [5; 3; 4; 4]

let simAB = cosineSimilarity a b
let simAC = cosineSimilarity a c
printfn "a & b: %A" simAB
printfn "a & c: %A" simAC

let ratings = [a; b; c]
printfn "%A" ratings

let k = 1. / (abs simAB + abs simAC)
k * predictRate 0 3 ratings
|> printfn "predicted (0, 3): %A"
