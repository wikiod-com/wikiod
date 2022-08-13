---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## The tabular environment
The `tabular` environment is the most basic way to create a table in LaTeX and doesn't require any other packages.
<!-- language: lang-latex -->

    \begin{tabular}{|lcr||}
      left aligned column & center column & right column \\
      \hline
      text & text & text \\
      text & text & text \\
    \end{tabular}

[![Rendered result][1]][1]

The parameter (`|lcr||` in the example) is called the **table specification** and tells LaTeX how many columns there are and how they are supposed to be formatted. Each letter represents a single column. Possible values are:

| Character               | Meaning |
| ----------------------- | ------  |
| l                       | left aligned column|
| c                       | centered column|
| r                       | right aligned column|
| p{'width'} e.g. `p{5cm}`| paragraph column with defined width|
| \| (pipe character)     | vertical line|
| \|\| (2 pipes)          | 2 vertical lines|

Cells are seperated by the `&` character. A row is ended by 2 back slashes `\\`.

Horizontal lines can be inserted by using the `\hline` command.


Tables are always formatted to be wide enough to include all the content. If a table is to big, LaTeX will print `overfull hbox` warnings. Possible solutions include using the `p{'width'}` specifier or other packages like `tabularx`.


A table with column headings spanning over several columns can be created using the command `\multicolumn{cols}{pos}{text}`.

    \begin{center}
    \begin{tabular}{|c|c|c|c|}
    \hline
    &\multicolumn{3}{|c|}{Income Groups}\\
    \cline{2-4}
    City&Lower&Middle&Higher\\
    \hline
    City-1& 11 & 21 & 13\\
    City-2& 21 & 31 &41\\
    \hline
    \end{tabular}
    \end{center}




[![Table with multicolumn headings][2]][2]

Note that the command `\multicolumn` has three mandatory arguments: the first argument specifies the number of columns over which the heading spans; the second argument specifies the position of the heading`(l,c,r)`; and the third argument is the text for heading. The command `\cline{2-4}` specifies the the starting column(here, 2) and ending column(here, 4) over which a line is to be drawn.

  [1]: http://i.stack.imgur.com/BNwOp.png
  [2]: https://i.stack.imgur.com/EEHSO.jpg

## Coloring Table
To make the table more readable, following are the ways to color it:

 1. Rows
 2. Columns
 3. Lines
 4. Cells

**Coloring Rows**

Use `\rowcolor` (provided by [`colortbl`](http://ctan.org/pkg/colortbl); also loaded by [`xcolor`](http://ctan.org/pkg/xcolor) under the `[table]` package option). Example: 

    \documentclass{article}
    \usepackage[table]{xcolor}
    
    \begin{document}

    \begin{tabular}{ | l | l | l | }
      \rowcolor{green}
      A & B & C \\
      \rowcolor{red}
      D & E & F \\
      G & H & I \\
      \rowcolor{blue}
      J & K & L
    \end{tabular}

    \end{document}

[![enter image description here][1]][1]

----------

**Coloring Columns**

Columns can be colored using following ways:
    
 - Defining column color property outside the table tag using `\newcolumntype`:
        
        \newcolumntype{a}{ >{\columncolor{yellow}} c }

 - Defining column color property inside the table parameters
    
        \begin{tabular}{ | >{\columncolor{red}} c | l | l }

Example: 

    \documentclass{article}
    \usepackage[table]{xcolor}
    
    \newcolumntype{a}{>{\columncolor{yellow}}c}
    \newcolumntype{b}{>{\columncolor{green}}c}
    
    \begin{document}

    \begin{tabular}{ a | >{\columncolor{red}}c | l | b }
      \hline
      A & B & C & D \\
      E & F & G & H \\
      \hline
    \end{tabular}

    \end{document}

[![enter image description here][2]][2]

----------

**Coloring Lines**

Use `\arrayrulecolor`. Example:

    \documentclass{article}
    \usepackage[table]{xcolor}
    
    \arrayrulecolor{blue}
    
    \begin{document}

    \begin{tabular}{ | l | l | l | }
      \hline
      A & B & C \\
      \hline
      D & E & F\\
      \hline
      G & H & I \\
      \hline
    \end{tabular}

    \end{document}

[![enter image description here][3]][3]

----------


**Coloring Cells**

Use `\cellcolor`. Example:

    \documentclass{article}
    \usepackage[table]{xcolor}

    \begin{document}

    \begin{tabular}{ | l | l | l | }
      \hline
      A & B & C \\
      \hline
      D & E & \cellcolor{green}F \\
      \hline
      G & H & I \\
      \hline
    \end{tabular}

    \end{document}

[![enter image description here][4]][4]

----------

We can define our own colors too using package `colortbl`. Following are the tags examples:

        \definecolor{Gray}{gray}{0.85}
        \columncolor[RGB]{230, 242, 255}}
        \columncolor[HTML]{AAACED}

  [1]: http://i.stack.imgur.com/LodcX.png
  [2]: http://i.stack.imgur.com/761ty.png
  [3]: http://i.stack.imgur.com/oiQjR.png
  [4]: http://i.stack.imgur.com/z7zN2.png

