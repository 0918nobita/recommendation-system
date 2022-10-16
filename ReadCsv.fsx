module ReadCsv

#load "Parsec.fsx"

open System.IO

open Parsec

let charsToString (chars: list<char>) =
    chars
    |> List.map string
    |> List.reduce (+)

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

let readCsvLines (filePath: string) (operation: float -> string * list<string> -> unit) =
    let bytes = FileInfo(filePath).Length

    use fs = new FileStream(filePath, FileMode.Open)

    use bs = new BufferedStream(fs)

    use sr = new StreamReader(bs)

    let firstLine = sr.ReadLine()
    if firstLine = null then
        ()
    else
        let mutable shouldContinue = true

        while shouldContinue do
            let line = sr.ReadLine()

            if line = null then
                shouldContinue <- false
            else
                match csvLine line with
                | Some (cells, "") ->
                    let pos = sr.BaseStream.Position
                    let progress = (float pos) / (float bytes) * 100.
                    operation progress cells
                | _ -> failwith "Invalid input"
