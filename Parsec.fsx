module Parsec

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
