module MarkdownDocument

type MarkdownDocument = list<MarkdownBlock>
    and MarkdownBlock = 
        | Heading of int * MarkdownSpans
        | Paragraph of MarkdownSpans
        | CodeBlock of list<string>

    and MarkdownSpans = list<MarkdownSpan>
    
    and MarkdownSpan =
        | Literal of string
        | InlineCode of string
        | HardlineBreak 
        | Strong of MarkdownSpans
        | Emphasis of MarkdownSpans
        | Hyperlink of MarkdownSpans * string


         //match lines.Head with
        //| h when h.Equals(System.String.IsNullOrWhiteSpace) -> // empty row found  could be a horizontal 
        //        None
        //| h when not(h.Equals(System.String.IsNullOrWhiteSpace)) -> 
        //       let possibleHeaders = [List.partialWhile(fun line-> headingLine '=' line) lines.Tail; List.partialWhile(fun line-> headingLine '-' line) lines.Tail]
        //       let rec headerLoop acc = function
        //       | ((a:string list),rest)::t when not(a.IsEmpty) -> Some(acc, a, rest)
        //       | ((a:string list),_)::t when a.IsEmpty -> headerLoop (acc+1) t
        //       //| ((a:string list),b)::[] when not(a.IsEmpty) -> Some(acc, a, b)
        //       | ((a:string list),_)::[] when a.IsEmpty -> None
        //       | []
        //       | _ -> None
        //       headerLoop 1 possibleHeaders
               