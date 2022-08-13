---
title: "Code"
slug: "code"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Syntax
- *inline*: \`Code between backticks\` *or* \<code>Code between these HTML tags\</code>
- *multiline*: \_\_\_\_Code that is indented on each line *or* \<pre>\<code>Multiple lines of code between these HTML tags\</code>\</pre>

## Inline code
Markdown supports adding inline code `like this`, obtained by wrapping text in backticks:

`` `code here` ``

Alternatively, you can put your inline code between `<code>` and `</code>` HTML tags.

Consider the following markdown code:

<!-- language: lang-none -->

    `This` is an inline code block! <code>This</code> is one too!

That would produce the following output:

> `This` is an inline code block! <code>This</code> is one too!

----

If you need to include a backtick inside inline code, you can use multiple backticks to begin and end the inline code block, like this:

    ``code containing a backtick (`) character``

That would produce the following output:

>``code containing a backtick (`) character``

----

Use `\` to escape backticks. For example:

    \`a\`

will be rendered as

\`a\`

## Syntax highlighting (StackExchange)
On StackExchange sites, code snippets may provide optional syntax highlighting. On sites like Stack Overflow the default language is derived from the tags used in the associated question (if applicable). In addition, a code snippet's syntax highlighting language may also be [defined by adding an HTML comment to the text body][1].

    <!-- language: lang-vb -->
    
        Sub ShowVB()
        Dim i As Long
        For i = 1 To 2
            If i = 3 Then
                MsgBox "How did that happen?"
            End If
        Next
        End Sub

Such a comment will change the syntax highlighting language for all subsequent code snippets, which can be rather useful, especially when several languages are involved in one post.

The above will be rendered with Visual Basic highlighting as:

<!-- language: lang-vb -->

    Sub ShowVB()
    Dim i As Long
    For i = 1 To 2
        If i = 3 Then
            MsgBox "How did that happen?"
        End If
    Next
    End Sub

 [1]: http://meta.stackexchange.com/questions/184108/what-is-syntax-highlighting-and-how-does-it-work/184109#184109

## Fenced Code Blocks
Some parsers allow code to be designated by adding three backticks before and after a section of code.

    ```
    <p><em>This</em> is an HTML example!</p>
    ```

Optionally, many parsers allow adding syntax highlighting by specifying the code's language immediately after the first set of backticks:

    ```html
    <p><em>This</em> is an HTML example!</p>
    ```

Result:

```html
<p><em>This</em> is an HTML example!</p>
```

## Indented code blocks inside lists
When adding indented code blocks inside a [list](https://www.wikiod.com/markdown/creating-lists) you first need a blank line, then to indent the code further. Different flavours of Markdown have different rules for this. 

1. StackExchange requires code to be indented by 8 characters instead of the usual 4. *(Spaces replaced with `*` for clarity)*:

        1.*Listitem1
        2.*Listitem2
        
        ********code here
        3.*Listitem3

2. Specs such as [CommonMark](http://commonmark.org) require that the code block be indented 4 characters from where the list item text starts.  *(Spaces replaced with `*` for clarity)*:

        1.****Listitem1
        2.****Listitem2
        
        **********code here
        3.****Listitem3

## Indented code blocks
You can create multiline code snippets by indenting each line with at least four spaces or one tab:

<!-- language: lang-none -->

        #include <stdio.h>

        int main() {
          printf("Hello World!\n");
          return 0;
        }


