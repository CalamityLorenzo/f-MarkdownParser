module MarkdownLoader

open System.IO

let getText path = 
    use sReader = new StreamReader(path=path)
    sReader.ReadToEnd()

let getTextAsync path = async {
     use sReader = new StreamReader(path=path)
     return sReader.ReadToEndAsync() |> Async.AwaitTask 
     }
// Sequence is lazilty evaluated
let getTextLines path = seq{
     use sReader = new StreamReader(path=path)
     while not sReader.EndOfStream do   
        yield sReader .ReadLine()
    }