module activePatternParser
open MarkdownDocument
open delimterActivePattern
// Parsing spans
let toString chars = 
  System.String(chars |> Array.ofList)

let rec parseSpans acc chars = seq{
    let emitLiteral = seq {
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal}
    
    match chars with // body is the successful result if the delimters accuraltey match.
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral
        yield InlineCode (toString body)
        yield! parseSpans [] chars
    | Delimited ['*';'*'] (body, chars)
    | Delimited ['_'; '_'](body, chars)->
        yield! emitLiteral
        yield Strong (parseSpans [] body |> List.ofSeq) // strong can contain other span types.
        yield! parseSpans [] chars
    | Delimited ['*'](body,chars)
    | Delimited ['_'](body,chars) ->
        yield! emitLiteral
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | ConcurrentDelimiter  ['[';']'] ['(';')']  (text,link, chars)-> // If there is a space between the two, you need to put that in the delimiters 
        yield! emitLiteral
        yield Hyperlink(parseSpans [] text |> List.ofSeq, link |> toString)
        yield! parseSpans [] chars
    | LineBreaks chars->
        yield! emitLiteral
        yield HardlineBreak
        yield! parseSpans [] chars
    | c::chars -> // no match to anything we are building at the moment so as you were
        yield! parseSpans (c::acc) chars
    | [] -> yield! emitLiteral // end of the line just check to see if there is a lurking literal anywhere.
}


let rec parseBlocks lines = seq{
    match lines with
    //| AsCharList (StartsWith ['#';' '] heading)::lines-> // This seems odd but is the same as line::lines further down. It's just wrapped in function or two.
    //    yield Heading(1, parseSpans [] heading |> List.ofSeq)
    //    yield! parseBlocks lines
    //| AsCharList (StartsWith ['#'; '#'; ' '] heading)::lines ->
    //    yield Heading(2, parseSpans [] heading |> List.ofSeq)
    //    yield! parseBlocks lines
    | line::lines when System.String.IsNullOrWhiteSpace(line)->
        yield! parseBlocks lines
    | HeadingUnderline (size, heading, lines)  ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    | HeadingHash (size,heading, lines) ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    | HorizontalRow (lines) ->
        printfn "HR"
        yield HorizontalRule
        yield! parseBlocks lines
    | PrefixedLines "    " (body, lines) when body <> [] -> // Complete match so no need to use the list syntax
        printfn "PL %s" (String.concat " " body)
        yield CodeBlock (body)
        yield! parseBlocks lines
    | BlockQuotes (quote, lines)  ->
        yield BlockQuote(parseBlocks quote |> Seq.toList)
        yield! parseBlocks lines
    | LineSeperated (body, lines) when body <> []->
        let body = String.concat " " body |> List.ofSeq
        yield Paragraph(parseSpans [] body |> List.ofSeq)
        yield! parseBlocks lines
    | _ -> ()
}
