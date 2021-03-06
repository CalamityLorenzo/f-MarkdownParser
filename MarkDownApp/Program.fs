﻿// Learn more about F# at http://fsharp.org
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
    let filenMae = @"\mkFiles\mk2.md"
    printfn "Hello World from F#! %s" filenMae
    let bum = "#### This is on tv" |> Seq.toArray
    let markdownFiles= __SOURCE_DIRECTORY__ + filenMae
    //let markdownLines = MarkdownLoader.getTextAsync markdownFiles  |>  Async.RunSynchronously |> System.Text.Encoding.ASCII.GetString 
    let markdownLines = (MarkdownLoader.getTextLines markdownFiles) |> Seq.toList
    let results = parseBlocks markdownLines
    Seq.iter (fun f -> printfn "%O" f ) results
    0 // return an integer exit code
