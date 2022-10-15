/// 2つのベクトルの内積
let dotProduct (vecA : list<int>) (vecB : list<int>) : int =
    List.map2 ( * ) vecA vecB
    |> List.sum

/// ベクトルの大きさ
let magnitude (vec : list<int>) : float =
    vec
    |> List.sumBy (fun elem -> elem * elem)
    |> float
    |> sqrt

/// 2つのベクトルのコサイン類似度
let cosineSimilarity (vecA : list<int>) (vecB : list<int>) : float =
    let magnitudeA = magnitude vecA
    let magnitudeB = magnitude vecB
    if magnitudeA = 0. || magnitudeB = 0. then
        -1.
    else
        float (dotProduct vecA vecB) / (magnitudeA * magnitudeB)

/// 加重平均
let weightedAverage (weights : seq<float>) (values : seq<float>) : float =
    let sumOfWeights = Seq.sum weights
    let sum =
        Seq.map2 ( * ) weights values
        |> Seq.sum
    sum / sumOfWeights

let predictRating (userIdx : int) (itemIdx : int) (userRatings: list<list<int>>) : float =
    let ratings =
        seq {
            for otherUserIdx in 0 .. List.length userRatings - 1 do
                if otherUserIdx <> userIdx then
                    let rating = userRatings.[otherUserIdx].[itemIdx]
                    yield float rating
        }
    let weights =
        seq {
            for otherUserIdx in 0 .. List.length userRatings - 1 do
                if otherUserIdx <> userIdx then
                    let cosSim = cosineSimilarity userRatings.[userIdx] userRatings.[otherUserIdx]
                    yield cosSim + 1.
        }
    weightedAverage weights ratings

let a = [1; 2; 5; 0]
let b = [2; 0; 4; 3]
let c = [5; 3; 4; 4]
let ratings = [a; b; c]
printfn "%A" ratings

let simAB = cosineSimilarity a b
let simAC = cosineSimilarity a c
printfn "a & b: %A" simAB
printfn "a & c: %A" simAC

predictRating 0 3 ratings
|> printfn "predicted (0, 3): %A"
