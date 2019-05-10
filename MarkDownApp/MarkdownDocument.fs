module MarkdownDocument

 type MarkdownSpans = list<MarkdownSpan>
    and MarkdownSpan =
        | Literal of string
        | InlineCode of string
        | HardlineBreak 
        | Strong of MarkdownSpans
        | Emphasis of MarkdownSpans
        | Hyperlink of MarkdownSpans * string

 type MarkdownContainer = 
        | Heading of int * MarkdownSpans
        | HorizontalRule
        | Paragraph of MarkdownSpans
        | CodeBlock of list<string>
        | BlockQuote of list<MarkdownContainer>
 type MarkdownLeaf =
        | UnorderedList of MarkdownSpans
        | OrderedList of int * MarkdownSpans

 type MarkdownBlock = MarkdownContainer | MarkdownLeaf
 type MarkdownDocument = list<MarkdownBlock>