---
title: "Build Tools"
slug: "build-tools"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Arara
[Arara] is a cross-platform automation tool that's specially designed for TeX.  It's included in a standard distribution, so there's no need to install anything additional.  It's most effectively understood as a means to record the compilation instructions in the TeX file itself:

```latex
% arara: pdflatex
\documentclass{article}
\begin{document}
  Hello, world
\end{document}
```

These can be much more complicated, though:

```latex
% arara: pdflatex
% arara: biber
% arara: pdflatex

% To support a self-contained example, this builds a BibTeX file on-the-fly
\begin{filecontents}{references.bib}
@article{dijkstra,
 author = {Dijkstra, Edsger W.},
 title = {Self-stabilizing Systems in Spite of Distributed Control},
 journal = {Commun. ACM},
 issue_date = {Nov. 1974},
 volume = {17},
 number = {11},
 month = nov,
 year = {1974},
 issn = {0001-0782},
 pages = {643--644},
 numpages = {2},
 url = {http://doi.acm.org/10.1145/361179.361202},
 doi = {10.1145/361179.361202},
 acmid = {361202},
 publisher = {ACM},
 address = {New York, NY, USA},
 keywords = {distributed control, error recovery, harmonious cooperation, multiprocessing, mutual exclusion, networks, robustness, self-repair, self-stabilization, sharing, synchronization},
}
\end{filecontents}


\documentclass{article}
\usepackage[backend=biber]{biblatex}
\addbibresource{references.bib}

\begin{document}
Hello, World! \cite{dijkstra}.
\printbibliography
\end{document}
```

[arara]:https://github.com/cereda/arara

