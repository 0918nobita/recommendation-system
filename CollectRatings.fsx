#load "ReadCsv.fsx"

#r "nuget: SQLitePCLRaw.bundle_e_sqlite3"
#r "nuget: Microsoft.Data.Sqlite.Core"

open Microsoft.Data.Sqlite

open ReadCsv

let createTable (conn: SqliteConnection) =
    use command = conn.CreateCommand()
    command.CommandText <- "CREATE TABLE IF NOT EXISTS ratings(user_id int, movie_id int, rating real)"
    ignore <| command.ExecuteNonQuery()

let () =
    use conn = new SqliteConnection("Data Source=test.sqlite")
    conn.Open()

    createTable conn

    let transaction = conn.BeginTransaction()

    use command = conn.CreateCommand()
    command.CommandText <- "INSERT INTO ratings(user_id, movie_id, rating) VALUES(@user_id, @movie_id, @rating)"

    let userIdParam = command.CreateParameter()
    userIdParam.ParameterName <- "@user_id"
    ignore <| command.Parameters.Add(userIdParam)

    let movieIdParam = command.CreateParameter()
    movieIdParam.ParameterName <- "@movie_id"
    ignore <| command.Parameters.Add(movieIdParam)

    let ratingParam = command.CreateParameter()
    ratingParam.ParameterName <- "@rating"
    ignore <| command.Parameters.Add(ratingParam)

    readCsvLines "ml-latest/ratings.csv" (fun progress cells ->
        match cells with
        | (userId, [movieId; rating; _timestamp]) ->
            userIdParam.Value <- userId
            movieIdParam.Value <- movieId
            ratingParam.Value <- rating
            ignore <| command.ExecuteNonQuery()

            printfn "Processing... (%f%%)" progress

        | _ -> failwith "Invalid input")

    transaction.Commit()
