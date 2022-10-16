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

let readCsvLines (fileReader: StreamReader) (operation: string * list<string> -> unit) =
    let firstLine = fileReader.ReadLine()
    if firstLine = null then
        ()
    else
        let mutable shouldContinue = true

        while shouldContinue do
            let line = fileReader.ReadLine()

            if line = null then
                shouldContinue <- false
            else
                match csvLine line with
                | Some (cells, "") -> operation cells
                | _ -> failwith "Invalid input"
