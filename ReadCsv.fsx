#r "nuget: SQLitePCLRaw.bundle_e_sqlite3"
#r "nuget: Microsoft.Data.Sqlite.Core"

open Microsoft.Data.Sqlite

open System.IO

let charsToString (chars: list<char>) =
    chars
    |> List.map string
    |> List.reduce (+)

type Parser<'a> = string -> option<'a * string>

let singleChar (cond: char -> bool) : Parser<char> =
    function
    | "" -> None
    | str ->
        let c = str.[0]
        if cond c then Some(c, str.[1..]) else None

let (.>>.) (parserA: Parser<'a>) (parserB: Parser<'b>) : Parser<'a * 'b> =
    fun str ->
        match parserA str with
        | Some (a, str) ->
            match parserB str with
            | Some(b, str) -> Some((a, b), str)
            | None -> None
        | None -> None

let (<|>) (parserA: Parser<'a>) (parserB: Parser<'a>) : Parser<'a> =
    fun str ->
        match parserA str with
        | Some res -> Some res
        | None -> parserB str

let (|>>) (parser: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    fun str ->
        match parser str with
        | Some (a, str) -> Some(f a, str)
        | None -> None

let many (parse: Parser<'a>) : Parser<list<'a>> = fun str ->
    let rec inner (acc: list<'a>) (str': string) =
        match parse str' with
        | Some (a, rest) -> inner (a :: acc) rest
        | None -> acc, str'

    let res, rest = inner [] str
    Some (List.rev res, rest)

let doubleQuoteInQuotedCell =
    singleChar ((=) '"')
    .>>. singleChar ((=) '"')
    |>> fst

let quotedCell =
    singleChar ((=) '"')
    .>>. many (doubleQuoteInQuotedCell <|> singleChar ((<>) '"'))
    .>>. singleChar ((=) '"')
    |>> fun ((_, content), _) -> charsToString content

let rawCell =
    many (singleChar ((<>) ','))
    |>> charsToString

let csvLine =
    (quotedCell <|> rawCell)
    .>>. many (singleChar ((=) ',') .>>. (quotedCell <|> rawCell) |>> snd)

let () =
    use conn = new SqliteConnection("Data Source=test.sqlite")
    conn.Open()
    use command = conn.CreateCommand()
    command.CommandText <- "CREATE TABLE IF NOT EXISTS movies(id int, title text)"
    ignore <| command.ExecuteNonQuery()

    use fs = new FileStream("ml-latest/movies.csv", FileMode.Open)

    use bs = new BufferedStream(fs)

    use sr = new StreamReader(bs)

    let firstLine = sr.ReadLine()

    if firstLine = null then
        printfn "The input file is empty."
        exit 0

    let mutable shouldContinue = true

    while shouldContinue do
        let line = sr.ReadLine()
        if line = null then
            shouldContinue <- false
        else
            printfn "%A" (Option.get (csvLine line))
