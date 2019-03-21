// Learn more about F# at http://fsharp.org
open MarkdownDocument
open activePatternParser
open delimterActivePattern
open System

let translateMarkspan spans = 
    let matchSpan = function
    | Literal s -> printfn "LITERAL: \n %s" (s.Replace ("\r\n", ""))
    | InlineCode c -> printfn "INLINE: \n %s" c
    | Strong c-> printfn "Strong" 
    | Emphasis c-> printfn "Emphasis" 
    | Hyperlink (c,l) -> printfn "HYPERLINK: %s" l
    | HardlineBreak -> printfn "HardLine"
    | _ -> printfn "Miscellaneous"    
    
    let rec checkSeq matchSe =
        match matchSe with
        | h::rest -> 
            matchSpan h
            checkSeq rest
        | [] ->()
    checkSeq spans

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let literalSpan = Literal "tony"
    let orig = "`Ln 1 code ln 2 Things are getting weidfred spaced` and **Welcome** to the `jungle`"
    let activeTest = "**i  \nmportant `code` ** and _emphas  \n\rized_ `jungle` [This is the `important  \ncode Sneak` part](http://hotmail) HOwever this conti  \rnues to be awuse"
    
    //let basicInline =  orig |> List.ofSeq |> parseInline 

    let morecode = activeTest |> List.ofSeq |> parseSpans [] |> List.ofSeq

    morecode |> Seq.toList |> translateMarkspan
    //let free = toTupleString basicInline
    //printfn "%s %s" free orig
    0 // return an integer exit code
