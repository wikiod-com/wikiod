---
title: "Regex Parsing"
slug: "regex-parsing"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
 - `new Regex(pattern);` //*Creates a new instance with a defined pattern.*
 - `Regex.Match(input);` //*Starts the lookup and returns the Match.*
 - `Regex.Matches(input);` //*Starts the lookup and returns a MatchCollection*


## Parameters
| Name | Details|
| ------ | ------ |
| Pattern | The `string` pattern that has to be used for the lookup. For more information: [msdn][1]|
| RegexOptions *[Optional]* | The common options in here are `Singleline` and `Multiline`. They are changing the behaviour of pattern-elements like the dot (.) which won't cover a `NewLine` (\n) in `Multiline-Mode` but in `SingleLine-Mode`. Default behaviour: [msdn][2] |
| Timeout *[Optional]* | Where patterns are getting more complex the lookup can consume more time. This is the passed timeout for the lookup just as known from network-programming.|


  [1]: https://msdn.microsoft.com/en-us/library/ae5bf541(v=vs.90).aspx
  [2]: https://msdn.microsoft.com/en-US/library/yd1hzczs(v=vs.110).aspx#Default

**Needed using**

    using System.Text.RegularExpressions;

**Nice to have**

  - You can test your patterns online without the need of compiling your solution to get results here: [Click me][1]    
  - Regex101 Example: [Click me][2]

_________

*Especially beginners are tended to overkill their tasks with regex because it feels powerful and in the right place for complexer text-based lookups. This is the point where people try to parse xml-documents with regex without even asking theirselfes if there could be an already finished class for this task like `XmlDocument`.*

*Regex should be the last weapon to pick agains complexity. At least dont forget putting in some effort to search for the `right way` before writing down 20 lines of patterns.*


  [1]: https://regex101.com/
  [2]: https://regex101.com/r/cG9lP5/1


## Single match
*`using System.Text.RegularExpressions;`*

    string pattern = ":(.*?):";
    string lookup = "--:text in here:--";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    // Get the match from your regex-object
    Match mLookup = rgxLookup.Match(lookup);
    
    // The group-index 0 always covers the full pattern.
    // Matches inside parentheses will be accessed through the index 1 and above.
    string found = mLookup.Groups[1].Value;

**Result:**  

    found = "text in here"

## Multiple matches
*`using System.Text.RegularExpressions;`*

    List<string> found = new List<string>();
    string pattern = ":(.*?):";
    string lookup = "--:text in here:--:another one:-:third one:---!123:fourth:";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    MatchCollection mLookup = rgxLookup.Matches(lookup);
    
    foreach(Match match in mLookup)
    {
        found.Add(match.Groups[1].Value);
    }

**Result:**  

    found = new List<string>() { "text in here", "another one", "third one", "fourth" }

