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
let (|LineBreaks|_|) input = 
    let a= System.String(input |> List.rev |> List.toArray)
    match input with
    | StartsWith (List.ofSeq "  \\n\\r") (rest)
    | StartsWith (List.ofSeq "  \\n") (rest)
    | StartsWith (List.ofSeq "  \\r") (rest) -> printfn"Success" ; Some(rest)
    | _ ->None

// Block level managementr
module List =
    let partialWhile f =
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs // When f(x) is true then (if it doesn't pass the next lines are not checked) (I think thats' the partial)
            | xs -> List.rev acc , xs // end result is a Tuple
        loop []
module String = 
    let All chr = 
        let rec loop = function
            | x::xs -> if x.Equals(chr) then 
                        loop xs
                       else
                        false
            | [] -> true
        loop
let headerTest line = 
    let rec headingLoop accNumber  = function
           | l::lne when l.Equals('#') -> 
            let acPLus = accNumber + 1
            printfn "ac Plus%d" acPLus
            headingLoop (acPLus) lne
           | l::lne when l.Equals(' ') -> Some(accNumber, lne)
           | [] 
           | _ -> None // These would be improperly formatted headingd
    headingLoop 0 line

let headingLine chr (line:string) = 
    if line.Length>2 && String.All chr (List.ofSeq line) then
        true
    else 
        false
    
let (|PrefixedLines|) (prefix:string) (lines:list<string>) = 
    let prefixed, other = 
        lines |> List.partialWhile (fun line-> line.StartsWith(prefix))
    [ for line in prefixed ->
        line.Substring(prefix.Length) ], other // remove the prefix from the line (That's a list  expression to make a tuple. Pithy) 

let (|LineSeperated|) lines =  // returns (list untilFirstWhiteSpace (or no whitespace), list afterWhitepsace)
    let isWhite = System.String.IsNullOrWhiteSpace 
    match List.partialWhile (isWhite >> not) lines with 
        | par, _::rest
        | par, ([] as rest) -> par, rest

let (|HeadingDash|_|) lines = 
    // Trying to find heading definitions === --- these appear UNDER another line. so we need the next line in a sequence/list
    // PartialWhile will allow me to match (first line with text, then the rest) if rest.[0] is - or = then we have a heading.
    let isWhite = System.String.IsNullOrWhiteSpace 
    match lines with
    | h::tail when not(h.Equals(System.String.IsNullOrWhiteSpace)) -> 
        match tail with // We only need to match the first entry.
            | line::tail -> 
                match line with
                    | l  when headingLine '=' l ->
                        Some(1,h |> List.ofSeq, tail)
                    | l when headingLine '-' l ->
                        Some(2, h |> List.ofSeq, tail)
                    | _ -> None
            | _ -> None
    | _ -> None
let (|HorizontalRow|_|) lines = 
    let isWhite = System.String.IsNullOrWhiteSpace 
    match lines with
    | h::tail when h.Equals(System.String.IsNullOrWhiteSpace) -> // empty row found  could be a horizontal 
                match tail with // We only need to match the first entry.
                | line::tail -> 
                    match line with
                        | l when headingLine '*' l || headingLine '-' l || headingLine '_' l -> Some(tail)
                        | _ -> None
                | _ -> None
    | _ -> None
let (|HeadingHash|_|) =function
         | (line:string)::rest when line.StartsWith("#") ->  // the line starts with a # but is it actually a valid headers
            match line |> List.ofSeq |> headerTest with
            | Some(size, heading) -> Some(size, heading, rest)
            | _ -> None
         | [] 
         | _ -> None

let (|AsCharList|) (str:string) =
    List.ofSeq str
