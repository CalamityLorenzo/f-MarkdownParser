# Introducing F#
F# is _functional-first_ language,
which looks like this:

    let msg = "world"
    printfn "hello %s" msg

This sample prints `hello world`

**important `code` ** and _emphasized_ `jungle` [This is the `important  \ncode Sneak` part](http://hotmail) HOwever this conti  \rnues to be nusiance


let testVar ="""# Introdu  \ncing F#
F# is _functional-first_ language,
which looks like this:

    let msg = "world"
    printfn "hello %s" msg

This sample prints `hello world`
===
**important `code` ** and _emphasized_ `jungle` [This is the `important  \ncode Sneak` part](http://hotmail) HOwever this conti  \rnues to be nusiance
---
# Hello
Frank this is the police  \r
Terry this is not the police""";;

testVar.Split('\r','\n') |> List.partialWhile (fun line->line.StartsWith("===") );;