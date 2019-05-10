module MarkdownLoader

open System.IO
open FSharp.Control.CommonExtensions

let getText path = 
    use sReader = new StreamReader(path=path)
    sReader.ReadToEnd()

let getTextAsync path = async {
     use fileReader = File.OpenRead(path=path) in
        return! fileReader.AsyncRead(fileReader.Length |>int)
     }

// Sequence is lazilty evaluated
let getTextLines path = seq{
     use sReader = new StreamReader(path=path)
     while not sReader.EndOfStream do   
        let lne = sReader.ReadLine()
        match lne.EndsWith("  ") with 
        | true -> yield lne + "\\r\\n"
        | false -> yield lne
    }   