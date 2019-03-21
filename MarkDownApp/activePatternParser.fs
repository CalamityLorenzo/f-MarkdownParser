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

