---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Tables are supported only in certain flavors of Markdown, including [Markdown Extra][1] and [Github Flavored Markdown][2], but not in the [original Markdown syntax][3] or in [CommonMark][4].

Markdown tables are also [not supported on Stack Exchange sites][5] (with the exception of [the Documentation beta][6]).

  [1]: https://michelf.ca/projects/php-markdown/extra/#table
  [2]: https://help.github.com/articles/organizing-information-with-tables/
  [3]: https://daringfireball.net/projects/markdown/syntax
  [4]: http://commonmark.org/help/
  [5]: http://meta.stackexchange.com/questions/73566/is-there-any-markdown-to-create-tables
  [6]: https://stackoverflow.com/


## Creating a table
Markdown tables are physically represented using dash `-` for to separate the header row from the content ones and pipe `|` for columns.

Column | Column
------ | ------
Cell   | Cell  

is produced by

    Column | Column
    ------ | ------
    Cell   | Cell  

You can also populate a table in any way you want -

Letter | Digit | Character
------ | ------|----------
a      | 4     | $
       | 365   | (
b      |       | ^  

That table's code:

    Letter | Digit | Character
    ------ | ------|----------
    a      | 4     | $
           | 365   | (
    b      |       | ^  

Markdown ignores spacing. The same table could be written like this:

    Letter|Digit|Character
    ---|---|---
    a|4|$
     |365|(
    b| |^  

and still display the same:

Letter|Digit|Character
---|---|---
a|4|$
 |365|(
b| |^  

**NOTE**: if you need a void column you must add a space between the pipes

    
As you can see, the code of the table does not need to represent the spacing of the table - that is accomplished within the markdown.

You should want to align the content of a table. All you have to do is add some colons in this way:

Aligning the column: 

`:` is used to align a column. Left align is the standard.

    Column | Column | Column
    :----- | :----: | -----:
    Left   | Center | Right
    align  | align  | align

Column | Column | Column
:----- | :----: | -----:
Left   | Center | Right
align  | align  | align


## Pipe in a cell content
If you want to use a pipe character (`|`) in the content of a cell you'll need to escape it with a backslash.


    Column | Column
    ------ | ------
    \| Cell \|| \| Cell \|  

This results in the following table:

Column | Column
------ | ------
\| Cell \|| \| Cell \|  

