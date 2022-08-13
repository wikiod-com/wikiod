---
title: "Creating a Bibliography"
slug: "creating-a-bibliography"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
- For a manually-formatted bibliography, there is no need to have citations - `\cite` - within the document.

## Parameters
| Parameter | Detail |
| ------ | ------ |
| `thebibliography`   | This environment sets the scope for the actual bibliography. It defines a list-like environment within which you can use `\bibitem` to set a bibliography item.   |
| `{x}` | The `thebibliography` environment takes a single argument that represents the widest element to be expected in the enumeration of the `\bibitem`s. For less than&nbsp;10 entries, use a single character/digit; for less than&nbsp;100 entries, use two characters/digits, ... |
| `\bibitem{<a>} <b>` | Set the bibliography item `<b>` and make it available to `\cite` within the document using the label `<a>`. 

## Basic bibliography with biber
To start a bibliography you need to define your sources. Create a [database file](http://www.bibtex.org/Format/) (like `sources.bib`) and include some content:

    @book{Doe1993,
        Author = {John Doe},
        Publisher = {Earth University},
        Title = {Creating a bibliography with biber},
        Year = {1993}}

You can then include your database file in your main document and cite the new source (`Doe1993`).

<!-- language: lang-latex -->

    \documentclass{article}
    
    % Include the biblatex package and tell it to use biber as a backend.
    % Without specifying the backend, it assumes biber.
    \usepackage[backend=biber]{biblatex}
    
    % Define where biber can find your sources
    \addbibresource{sources.bib}
    
    \begin{document}
    "Biber isn't that difficult." \cite{Doe1993}
    % Use \cite{source-ID} to generate a citation
    
    % Print the bibliography
    \printbibliography
    
    \end{document}

To compile the document, you will need to run 3 commands in sequence:

 1. `pdflatex` to create an auxiliary file which tells biber what sources are needed
 2. `biber` to create an auxiliary file with all the sources which can be used by `pdflatex`
 3. `pdflatex` to include the auxiliary file and create the PDF

[![Result][1]][1]

Find many more options and additional fields for bib files in the [package documentation on CTAN](http://ctan.org/pkg/biblatex).


  [1]: http://i.stack.imgur.com/tOLba.png

## Basic bibliography without packages (manual formatting)
[![enter image description here][1]][1]

<!-- language: lang-latex -->

    \documentclass{article}% or book, report, ...

    \begin{document}

    See \cite{citeA} or \cite{citeB} or \cite{citeA, citeB}.

    \begin{thebibliography}{x}
      % \bibitem{<biblabel>} <citation>
      \bibitem{citeA}
        {\scshape Author, A}, {\itshape A title}, Journal of So-and-So, 2000.
      \bibitem{citeB}
        {\scshape Someone, B}, {\itshape Another title}, Book of books, 1900.
    \end{thebibliography}

    \end{document}

Note that unless you really know *why*, you should probably not do this. Using designated packages (see other examples) is preferable.


  [1]: http://i.stack.imgur.com/FAoww.png

