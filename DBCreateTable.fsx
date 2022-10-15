#r "nuget: SQLitePCLRaw.bundle_e_sqlite3"
#r "nuget: Microsoft.Data.Sqlite.Core"

open Microsoft.Data.Sqlite

let () =
    use conn = new SqliteConnection("Data Source=test.sqlite")
    conn.Open()
    use command = conn.CreateCommand()
    command.CommandText <- "CREATE TABLE IF NOT EXISTS users(id int, name text)"
    ignore <| command.ExecuteNonQuery()
