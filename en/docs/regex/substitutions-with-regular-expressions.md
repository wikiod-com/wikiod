---
title: "Substitutions with Regular Expressions"
slug: "substitutions-with-regular-expressions"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
| Inline  | Description              |
| ------  | ------------------------ |
| $number | Substitutes the substring matched by group number. |
| ${name}   | Substitutes the substring matched by a [named group](https://www.wikiod.com/regex/capture-groups#Named Capture Groups) name. |
| $$      | Escaped '$' character in the result (replacement) string. |
| $&  *($0)*    | Replaces with the whole matched string. |
| $+ *($&)*  | Substitutes the matched text to the last group captured. |
| $`      | Substitutes all the matched text with every non-matched text before the match. |
| $'      | Substitutes all the matched text with every non-matched text after the match. |
| $_      | Substitutes all the matched text to the entire string.
|**Note:**| *Italic* terms means the strings are volatile (May vary depending on your regex flavor).|

## Basics of Substitution
One of the most common and useful ways to replace text with regex is by using [Capture Groups](https://www.wikiod.com/regex/capture-groups#Basic Capture Groups).<br>
Or even a [Named Capture Group](https://www.wikiod.com/regex/capture-groups#Named Capture Groups), as a reference to store, or replace the data.

There are two terms pretty look alike in regex's docs, so it may be important to never mix-up **Substitutions** (i.e. `$1`) with **[Backreferences](https://www.wikiod.com/regex/back-reference)** (i.e. `\1`). Substitution terms are used in a replacement text; Backreferences, in the pure Regex expression. Even though some programming languages accept both for substitutions, it's not encouraging.

Let's we say we have this regex: `/hello(\s+)world/i`. Whenever `$number` is referenced (in this case, `$1`), the whitespaces matched by `\s+` will be replaced instead. <br>The same result will be exposed with the regex: `/hello(?<spaces>\s+)world/i`. And as we have a named group here, we can also use `${spaces}`.

In this same example, we can also use `$0` or `$&` (**Note:** `$&` may be used as `$+` instead, meaning to retrieve the **LAST** capture group in other regex engines), depending on the regex flavor you're working with, to get the whole matched text. (i.e. `$&` shall return `hEllo woRld` for the string: `hEllo woRld of Regex!`)
<hr>
Take a look at this simple example of substitution using John Lennon's adapted quote by using the <code>$number</code> and the <code>${name}</code> syntax:<br><br>

**Simple capture group example:**
[![Substitution Example 1][1]][1]

**Named capture group example:**

[![Substitution Example 2][2]][2]


  [1]: https://i.stack.imgur.com/PzHql.png
  [2]: https://i.stack.imgur.com/uie5e.png

## Advanced Replacement
Some programming languages have its own Regex peculiarities, for example, the `$+` term (in C#, Perl, VB etc.) which replaces the matched text to the last group captured.

**Example:**

    using System;
    using System.Text.RegularExpressions;
    
    public class Example
    {
       public static void Main()
       {
          string pattern = @"\b(\w+)\s\1\b";
          string substitution = "$+";
          string input = "The the dog jumped over the fence fence.";
          Console.WriteLine(Regex.Replace(input, pattern, substitution, 
                            RegexOptions.IgnoreCase));
       }
    }
    // The example displays the following output:
    //      The dog jumped over the fence.

<sub>Example from Microsoft Official's Developer Network <sup>[[1]](https://msdn.microsoft.com/en-us/library/ewy2t5e0(v=vs.110).aspx?cs-save-lang=1&cs-lang=csharp#Anchor_7)</sup></sub>
<hr>
Other rare substitution terms are <code>$`</code> and <code>$'</code>:

<code>$`</code> = Replaces matches to the text **before** the matching string
<br><code>$'</code> = Replaces matches to the text **after** the matching string

Due to this fact, these replacements strings should do their work like this:

    Regex: /part2/
    Input: "part1part2part3"
    Replacement: "$`"
    Output: "part1part1part3" //Note that part2 was replaced with part1, due &` term
    ---------------------------------------------------------------------------------
    Regex: /part2/
    Input: "part1part2part3"
    Replacement: "$'"
    Output: "part1part3part3" //Note that part2 was replaced with part3, due &' term

Here is an example of these substitutions working on a piece of javascript:

    var rgx = /middle/;
    var text = "Your story must have a beginning, middle, and end"
    console.log(text.replace(rgx, "$`")); 
    //Logs: "Your story must have a beginning, Your story must have a beginning, , and end"
    console.log(text.replace(rgx, "$'"))
    //Logs: "Your story must have a beginning, , and end, and end"
<hr>
There is also the term <code>$_</code> which retrieves the whole matched text instead:

    Regex: /part2/
    Input: "part1part2part3"
    Replacement: "$_"
    Output: "part1part1part2part3part3" //Note that part2 was replaced with part1part2part3,
                                                                             // due $_ term

Converting this to VB would give us this:

    Imports System.Text.RegularExpressions
    
    Module Example
       Public Sub Main()
          Dim input As String = "ABC123DEF456"
          Dim pattern As String = "\d+"
          Dim substitution As String = "$_"
          Console.WriteLine("Original string:          {0}", input)
          Console.WriteLine("String with substitution: {0}", _
                            Regex.Replace(input, pattern, substitution))      
       End Sub
    End Module
    ' The example displays the following output:
    '       Original string:          ABC123DEF456
    '       String with substitution: ABCABC123DEF456DEFABC123DEF456

<sub>Example from Microsoft Official's Developer Network <sup>[[2]](https://msdn.microsoft.com/en-us/library/ewy2t5e0(v=vs.110).aspx?cs-save-lang=1&cs-lang=csharp#Anchor_8)</sup></sub>

<hr>

And the last but not least substitution term is `$$`, which translated to a regex expression would be the same as `\$` (An escaped version of the literal **$**).

If you want to match a string like this: `USD: $3.99` for example, and want to store the `3.99`, but replace it as `$3.99` with only one regex, you may use:

    Regex: /USD:\s+\$([\d.]+)/
    Input: "USD: $3.99"
    Replacement: "$$$1"
    To Store: "$1"
    Output: "$3.99"
    Stored: "3.99"

If you want to test this with Javascript, you may use the code:

    var rgx = /USD:\s+\$([\d.]+)/;
    var text = "USD: $3.99";
    var stored = parseFloat(rgx.exec(text)[1]);
    console.log(stored); //Logs 3.99
    console.log(text.replace(rgx, "$$$1")); //Logs $3.99

<hr>
<b>References</b>

[1]<b/>: [Substituting the Last Captured Group](https://msdn.microsoft.com/en-us/library/ewy2t5e0(v=vs.110).aspx?cs-save-lang=1&cs-lang=csharp#Anchor_7)
<br>[2]<b/>: [Substituting the Entire Input String](https://msdn.microsoft.com/en-us/library/ewy2t5e0(v=vs.110).aspx?cs-save-lang=1&cs-lang=csharp#Anchor_8)

