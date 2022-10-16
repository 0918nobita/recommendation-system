#load "ReadCsv.fsx"

#r "nuget: SQLitePCLRaw.bundle_e_sqlite3"
#r "nuget: Microsoft.Data.Sqlite.Core"

open Microsoft.Data.Sqlite

open ReadCsv

let createTable (conn: SqliteConnection) =
    use command = conn.CreateCommand()
    command.CommandText <- "CREATE TABLE IF NOT EXISTS movies(id int, title text)"
    ignore <| command.ExecuteNonQuery()

let () =
    use conn = new SqliteConnection("Data Source=test.sqlite")
    conn.Open()

    createTable conn

    let transaction = conn.BeginTransaction()

    use command = conn.CreateCommand()
    command.CommandText <- "INSERT INTO movies(id, title) VALUES(@id, @title)"

    let idParam = command.CreateParameter()
    idParam.ParameterName <- "@id"
    ignore <| command.Parameters.Add(idParam)

    let titleParam = command.CreateParameter()
    titleParam.ParameterName <- "@title"
    ignore <| command.Parameters.Add(titleParam)

    readCsvLines "ml-latest/movies.csv" (fun progress cells ->
        match cells with
        | (movieId, [title; _genre]) ->
            idParam.Value <- movieId
            titleParam.Value <- title
            ignore <| command.ExecuteNonQuery()

            printfn "Processing... (%f%%)" progress

        | _ -> failwith "Invalid input")

    transaction.Commit()
