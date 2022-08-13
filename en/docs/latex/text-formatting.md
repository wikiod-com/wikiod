---
title: "Text Formatting"
slug: "text-formatting"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Emphazise Text
In order to emphasize text the command `\emph` can be used which usually displays the text in an italics font:

    This is some text with \emph{emphasized words}.

## Strike through text
The command `\sout` of the package `ulem` strikes through a text:

    \sout{This text is striked through}

The package `ulem` redefines the command `\emph`. When you do not want to have this behavior you can use the package `ulem` with the option `normalem`:

    \usepackage[normalem]{ulem}

## Bold text
In order to typeset text in bold, use `\textbf`:

    \textbf{This text is typeset in bold.}


