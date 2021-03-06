﻿module delimterActivePattern

// Span management 
let (|StartsWith|_|) prefix input = 
    let rec loop = function
        | p::prefix, r::rest when p=r -> loop (prefix, rest)
        | [] , rest-> Some(rest) // reached the end of the prefix, and all is well.
        | _ -> None
    loop (prefix, input)

let rec parseBracketedBody closing acc = function  // Three paramters paul!!!! not two.
    | StartsWith closing (rest) ->
        Some(List.rev acc, rest) // the contained body, the rest of the stream
    | c::chars ->
        parseBracketedBody closing (c::acc) chars
    | _ -> None

let parseBracketed opening closing = function
    | StartsWith opening chars -> // match the front now get to the end.
        parseBracketedBody closing [] chars
    | _ -> None

// finally wrap up all the above in a paramterised Active Pattern
// Delimited takes a list (string is a list of chars) What goes in must be the same either side. yah know deliomted.
let (|Delimited|_|) delim = parseBracketed delim delim // partial function application

let (|Link|_|) input = 
    match parseBracketed ['['] [']'] input with
    | Some(text, restAfterLink)-> 
        match parseBracketed ['('] [')'] restAfterLink with
        | Some (link, rest)-> Some(text, link, rest)
        | _ -> None // We had a link but no text afterwards. so send everyone home.
    | _ -> None
               
let halveArray (arrayData:'a list) = 
    if arrayData.Length%2 <> 0 then
        failwith "Array must be divisible by 2!"
    let splits = arrayData.Length/2
    let lowerSplit = arrayData.[0..splits-1]
    let higherSplit = arrayData.[splits..arrayData.Length-1]
    (lowerSplit , higherSplit)
// first is opening and closing of first
// second is opening and lcosing of second. 
// can 1 or more chars in length
let (|ConcurrentDelimiter|_|) first second input = 
    let (firstOpen, firstClose) = halveArray first
    match parseBracketed firstOpen firstClose input with
        | Some(text, rest)->
            let (secondOpen, secondClose) = halveArray second
            match parseBracketed secondOpen secondClose rest with
            | Some(link, rest) -> Some(text,link, rest)
            | _ -> None
        | _ -> None
let (|LineBreaks|_|) input = 
    match input with    
    | StartsWith (List.ofSeq "  \r\n") (rest)
    | StartsWith (List.ofSeq "  \n") (rest)
    | StartsWith (List.ofSeq "  \r") (rest)
    | StartsWith (List.ofSeq "  
    \\n\\r") (rest)
    | StartsWith (List.ofSeq "  \\r\\n") (rest) 
    | StartsWith (List.ofSeq "  \\n") (rest)
    | StartsWith (List.ofSeq "  \\r") (rest) ->Some(rest)
    | _ -> None

// Block level managementr
module List =
    let partitionWhile f =
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs // When f(x) is true then (if it doesn't pass the next lines are not checked) (I think thats' the partial)
            | xs -> List.rev acc , xs // end result is a Tuple
        loop []
// checks a value and eats the line at the same time. 
module String = 
    let All chr = 
        let rec loop = function
            | x::xs when x.Equals(chr) -> loop xs
            | xs -> false 
            | [] -> true
        loop

let headerCalculate line = 
    let headerPartion = List.partitionWhile (fun f-> f='#') line
    match headerPartion with // 
     | (splats, txt) when splats.Length < 7 -> Some(splats.Length, txt.Tail)
     | _ -> None
     
let isHeadingLine chr (line:string) = 
     line.Length>2 && String.All chr (List.ofSeq line)

let (|PrefixedLines|) (prefix:string) (lines:list<string>) = 
    let prefixed, other = 
        lines |> List.partitionWhile (fun line-> line.StartsWith(prefix))
    [ for line in prefixed ->
        line.Substring(prefix.Length) ], other // remove the prefix from the line (That's a list  expression to make a tuple. Pithy) 

let (|LineSeperated|) lines =  // returns (list untilFirstWhiteSpace (or no whitespace), list afterWhitepsace)
    let isWhite = System.String.IsNullOrWhiteSpace 
    match List.partitionWhile (isWhite >> not) lines with 
     | par, _::rest // This discards the first entry in the remaining column.
     | par, ([] as rest) -> par, rest

let matchBlockQuote lines =
    let isBlock = "> "
    match List.partitionWhile(fun (i:string)-> i.StartsWith(isBlock)) lines with 
      | par, others-> par, others
    
let (|HeadingUnderline|_|) lines = 
    // Trying to find heading definitions === --- these appear UNDER another line. so we need the next line in a sequence/list
    // PartialWhile will allow me to match (first line with text, then the rest) if rest.[0] is - or = then we have a .heading
    let isWhite = System.String.IsNullOrWhiteSpace 
    match lines with
    | h::tail when not(h.Equals(isWhite)) -> 
        match tail with // We only need to match the first entry.
            | line::tail -> 
                match line with
                    | l  when isHeadingLine '=' l ->
                        Some(1,h |> List.ofSeq, tail)
                    | l when isHeadingLine '-' l ->
                        Some(2, h |> List.ofSeq, tail)
                    | _ -> None
            | _ -> None
    | _ -> None
let (|HorizontalRow|_|) lines = 
    let isWhite = System.String.IsNullOrWhiteSpace 
    match lines with
    | h::tail when h.Equals(isWhite) -> // empty row found  could be a horizontal 
                match tail with // We only need to match the first entry.
                | line::tail -> 
                    match line with
                        | l when isHeadingLine '*' l || isHeadingLine '-' l || isHeadingLine '_' l -> Some(tail)
                        | _ -> None
                | _ -> None
    | _ -> None
let (|HeadingHash|_|) =function
         | (line:string)::rest when line.StartsWith("#") ->  // the line starts with a # but is it actually a valid headers
            match line |> List.ofSeq |> headerCalculate with
            | Some(size, heading) -> Some(size, heading, rest)
            | _ -> None
         | [] 
         | _ -> None
let (|BlockQuotes|_|) (lines:list<string>) = 
    match lines with 
    | lne::_ when lne.StartsWith "> " -> 
        let quote, rest = (matchBlockQuote lines)
        Some([for line in quote ->
                line.Substring 2
        ], rest)
    | _ -> None
let (|AsCharList|) (str:string) =
    List.ofSeq str
