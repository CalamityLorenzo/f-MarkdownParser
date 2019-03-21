module basicRecursiveParser
open MarkdownDocument

let rec parseInlineBody  acc = function 
    | '`'::rest ->
        Some(List.rev acc, rest) // all we processed and everything left over
    | c::chars ->
        parseInlineBody (c::acc) chars // keep consuming
    | []-> None //Sould not reach here, missing ending backitick

let parseInline = function
    | '`'::chars ->
        parseInlineBody [] chars
    | _ -> None // No match for start of inline code markdown so away we go.

    // Parsing spans
let toString chars = 
      System.String(chars |> Array.ofList)

      // What to expect if we follow the recursive path
      // It's not very good. at All
let rec parseSpans acc chars = seq {
    let emitLiteral = seq{
        if acc <> [] then   
            yield acc |> List.rev |> toString |> Literal}

    match (parseInline chars, chars) with // A jerry rigged tuple.
        | Some (body, restoChars), _-> // If we have Something, that is  an inlineCode thingy. (ignore the second chars)
            yield! emitLiteral
            yield body |> toString |> InlineCode
            yield! parseSpans [] restoChars
        | _, c::chars -> // No InlineCode found, so check for spans.
            yield! parseSpans (c::acc) chars
        | _, [] -> // No Inline Code, and nothing in the chars list at present so emit literal from the accumluator.
            yield! emitLiteral
}
