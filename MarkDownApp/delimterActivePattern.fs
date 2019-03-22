module delimterActivePattern

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
let (|LineBreaks|_|) = function
    | StartsWith (List.ofSeq "  \n\r") (rest)
    | StartsWith (List.ofSeq "  \n") (rest)
    | StartsWith (List.ofSeq "  \r") (rest) -> Some(rest)
    | _ ->None

// Block level managementr
module List =
    let partialWhile f =
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs // When f(x) is true then
            | xs -> List.rev acc , xs // end result is a Tuple
        loop []

let headingCheck line = 
    let rec headingLoop accNumber  = function
           | l::lne when l.Equals('#') -> headingLoop (accNumber +  1) lne
           | l::lne when l.Equals(' ') -> Some(accNumber, lne)
           | [] 
           | _ -> None // These would be improperly formatted headingd

    headingLoop 0 line

let (|PrefixedLines|) (prefix:string) (lines:list<string>) = 
    let prefixed, other = 
        lines |> List.partialWhile (fun line-> line.StartsWith(prefix))
    [ for line in prefixed ->
        line.Substring(prefix.Length) ], other // remove the prefix from the line (That's an expression to make a tuple. Pithy)

let (|LineSeperated|) lines =  // returns (list untilFirstWhiteSpace (or no whitespace), list afterWhitepsace)
    let isWhite = System.String.IsNullOrWhiteSpace 
    match List.partialWhile (isWhite >> not) lines with 
        | par, _::rest
        | par, ([] as rest) -> par, rest

let (|HeadingTest|_|) =function
         | (line:string)::rest when line.StartsWith('#') -> 
            match line |> List.ofSeq |> headingCheck with
            | Some(size, heading) -> Some(size, heading, rest)
            | _ -> None
         | [] 
         | _ -> None

let (|AsCharList|) (str:string) =
    List.ofSeq str
