#load "ReadCsv.fsx"

#r "nuget: SQLitePCLRaw.bundle_e_sqlite3"
#r "nuget: Microsoft.Data.Sqlite.Core"

open Microsoft.Data.Sqlite
open System.IO

open ReadCsv

let createTable (conn: SqliteConnection) =
    use command = conn.CreateCommand()
    command.CommandText <- "CREATE TABLE IF NOT EXISTS movies(id int, title text)"
    ignore <| command.ExecuteNonQuery()

let () =
    use conn = new SqliteConnection("Data Source=test.sqlite")
    conn.Open()

    createTable conn

    use fs = new FileStream("ml-latest/movies.csv", FileMode.Open)

    use bs = new BufferedStream(fs)

    use sr = new StreamReader(bs)

    let transaction = conn.BeginTransaction()

    use command = conn.CreateCommand()
    command.CommandText <- "INSERT INTO movies(id, title) VALUES(@id, @title)"

    let idParam = command.CreateParameter()
    idParam.ParameterName <- "@id"
    ignore <| command.Parameters.Add(idParam)

    let titleParam = command.CreateParameter()
    titleParam.ParameterName <- "@title"
    ignore <| command.Parameters.Add(titleParam)

    readCsvLines sr (fun cells ->
        match cells with
        | (id_, [title; _genre]) ->
            idParam.Value <- int id_
            titleParam.Value <- title
            ignore <| command.ExecuteNonQuery()

        | _ -> failwith "Invalid input")

    transaction.Commit()
