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
    let orig = "`Ln 1 code ln 2 Things are getting weidfred space d` and **Welcome** to the `jungle`"
    let activeTest = """# Introducing F#
    F# is _functional-first_ language,
    which looks like this:
    
        let msg = "world"
        printfn "hello %s" msg
    
    This sample prints `hello world`
    
    **important `code` ** and _emphasized_ `jungle` [This is the `important  \ncode Sneak` part](http://hotmail) HOwever this conti  \rnues to be nusiance
    """
    
    //let basicInline =  orig |> List.ofSeq |> parseInline 

    let morecode = activeTest |> List.ofSeq |> parseSpans [] |> List.ofSeq
    let f = match "Mr. Plinkett’s Transformers: The Last Knight Review" with
            | AsCharList b->b

    printfn "%d" f.Length
    
    let d =  f.[0..3]
    printfn "%s" (String.Join("",d))
    
    morecode |> Seq.toList |> translateMarkspan
    //let free = toTupleString basicInline
    //printfn "%s %s" free orig
    0 // return an integer exit code
