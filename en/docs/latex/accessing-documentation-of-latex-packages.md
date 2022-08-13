---
title: "Accessing documentation of LaTeX packages"
slug: "accessing-documentation-of-latex-packages"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## CTAN
The [Comprehensive TeX Archive Network](http://www.ctan.org/) (CTAN) is indeed that, *the* comprehensive repository of LaTeX packages. Most if not all quality packages (and more) are on there, and all the good ones include documentation.

 1. Enter the package name into the search bar.

    [![enter image description here][1]][1]

 2. Select the package from the list.

    [![enter image description here][2]][2]

 3. Access the documentation documents.

    [![enter image description here][3]][3]

**Important:** CTAN holds the most recent versions. If your installation is outdated the documentation won't match! In that case, refer to the documentation documents shipped with your LaTeX distribution.


  [1]: http://i.stack.imgur.com/2JRQp.png
  [2]: http://i.stack.imgur.com/0KhqA.png
  [3]: http://i.stack.imgur.com/Hnorr.png

## TeX Live -- texdoc
If you use the [TeX Live](http://tug.org/texlive/) distribution you can use the command-line program `texdoc`. For instance,

<!-- languauge: lang-bash -->
```
texdoc biblatex
```

will open the documentation of package `biblatex`.

Or if you are not command-line-savvy, the same can be found online at http://www.texdoc.net/ 

