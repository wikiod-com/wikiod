---
title: "DialectsFlavours"
slug: "dialectsflavours"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

Variations of Markdown with differing syntax are called "flavors." Markdown flavors are listed at https://github.com/jgm/CommonMark/wiki/Markdown-Flavors. 

A Markdown flavor can be implemented in various programming languages and software applications. Libraries that implement a Markdown parser are listed at https://github.com/markdown/markdown.github.com/wiki/Implementations and https://www.w3.org/community/markdown/wiki/MarkdownImplementations.

Several Markdown flavors, and the differences between them, are documented at http://flavoredmarkdown.com. 

To test and compare Markdown code in many different flavors, and implementations of those flavors, you can use the online tool [Babelmark](https://babelmark.github.io).

## Stack Overflow Markdown
This is the flavor of markdown that's used by Stack Overflow and other Stack Exchange sites. When you answer a question or add documentation you use this markdown. This answer is made out of SO markdown

See [Official Documentation][1]

---

The main things that SO markdown adds are under "Stack Exchange additions" on that webpage. In particular, SO adds *tags* like `[tag:tag]` and shortcut links like `[meta]` (not in docs though), *spoilers*:

>! This is a spoiler

    >! This is a spoiler

and custom language devotions

    <!-- language: java -->
    ```
    This text is formatted as if it were Java code
    ```
    
<!-- language: java -->
```
This text is formatted as if it were Java code
```

  [1]: http://stackoverflow.com/editing-help

## Doxygen Markdown
[Doxygen](http://www.stack.nl/~dimitri/doxygen/index.html) is a commonly used code documentation tool (for languages including C++, C# and Java) that also supports the use of Markdown. In addition to the standard Markdown syntax, there are a number of [Doxygen-specific elements](https://www.stack.nl/~dimitri/doxygen/manual/markdown.html#markdown_dox).

The primary features are the use of `@ref` tags for references, and the `@page`, `@section/@subsection` and `@anchor` elements that these can reference.

## @ref \<name\> ["text"]

This element creates a link (ie. reference) to a named section, subsection, page or anchor that has been defined elsewhere in the documentation. (see [Doxygen reference](https://www.stack.nl/~dimitri/doxygen/manual/commands.html#cmdref))

The first parameter (`name`) should match the name of the section, subsection, page or anchor that you wish to link to.

The second optional parameter (`"text"`) should be encapsulated in double quotes, and will define what the reference link will appear as on the page. If not used, the link will appear as the title used in the reference.

## @section \<section-name\> (section title)

This element defines a section name. It is visually equivalent to a `#` element in Markdown, however it will also define a reference that can be linked to from other sections of your documentation. (see [Doxygen reference](https://www.stack.nl/~dimitri/doxygen/manual/commands.html#cmdsection))

The first parameter `section-name` defines the reference name that can be used by the `@ref` element. This cannot contain any spaces.

The second parameter `section title` is a string of words (that can be separated by spaces) which defines what the section heading will appear as on your page.

## Example

```{.md}
@section Intro Introduction

This is some text in my introduction.

@section Body Body Paragraph

This is some text in my body, where I refer to the @ref Intro.
```

### Output

# Introduction

This is some text in my introduction.

# Body Paragraph

This is some text in my body, where I refer to the Introduction.

*NB: The word Introduction above will appear as a link that will jump to the Introduction heading.*

## GitHub Flavored Markdown
[GitHub Flavored Markdown][1] (sometimes abbreviated to GFM) makes it easier to work with markdown on [GitHub.com][2].

Key features of GFM include:
- code indentation
- task list support
- easy GitHub issue referencing
- automatic GitHub username and SHA detection
- automatic url detection
- emoji support

# GFM examples
## Syntax highlighting
With Markdown, a block of code can be generated with three backticks:
> `` ``` ``

Without syntax highlighting, code written in C appears like this
```
#include <stdio.h>

int main(void)
{
    printf("Hello World\n");
    return (0);
}
```

However, ***with*** syntax highlighting, code written in C appears like this:
[![screenshot of GFM syntax highlighting][3]][3]

In order to highlight code, simply add an optional language identifier to enable syntax highlighting in your fenced code block. 

```` 
```C
<code text here>
```
````

These enhancements are designed to improve the quality of documentation and conversation included in `README` files, [gists][4], [pull requests][5] and [issues][6] on the platform.


  [1]: https://guides.github.com/features/mastering-markdown/#GitHub-flavored-markdown
  [2]: https://github.com
  [3]: https://i.stack.imgur.com/TbEwA.png
  [4]: https://help.github.com/articles/about-gists/
  [5]: https://help.github.com/articles/about-pull-requests/
  [6]: https://help.github.com/articles/about-issues/

