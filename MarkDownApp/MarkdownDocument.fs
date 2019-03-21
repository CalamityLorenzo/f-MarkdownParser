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
