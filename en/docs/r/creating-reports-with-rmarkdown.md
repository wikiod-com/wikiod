---
title: "Creating reports with RMarkdown"
slug: "creating-reports-with-rmarkdown"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Including bibliographies
A bibtex catalogue cna easily be included with the YAML option `bibliography:`. A certain style for the bibliography can be added with `biblio-style:`.
The references are added at the end of the document.

    ---
    title: "Including Bibliography"
    author: "John Doe"
    output: pdf_document
    bibliography: references.bib
    ---
    
    # Abstract
    
    @R_Core_Team_2016
    
    # References


[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/WRoNAm.png

## Printing tables
There are several packages that allow the output of data structures in form of HTML or LaTeX tables. They mostly differ in flexibility.

Here I use the packages:

 - knitr
 - xtable
 - pander 

**For HTML documents**

    ---
    title: "Printing Tables"
    author: "Martin Schmelzer"
    date: "29 Juli 2016"
    output: html_document
    ---
    
    ```{r setup, include=FALSE}
    knitr::opts_chunk$set(echo = TRUE)
    library(knitr)
    library(xtable)
    library(pander)
    df <- mtcars[1:4,1:4]
    ```
    
    # Print tables using `kable`
    ```{r, 'kable'}
    kable(df)
    ```
    
    # Print tables using `xtable`
    ```{r, 'xtable', results='asis'}
    print(xtable(df), type="html")
    ```
    
    # Print tables using `pander`
    ```{r, 'pander'}
    pander(df)
    ```

[![enter image description here][1]][1]




**For PDF documents**

    ---
    title: "Printing Tables"
    author: "Martin Schmelzer"
    date: "29 Juli 2016"
    output: pdf_document
    ---
    
    ```{r setup, include=FALSE}
    knitr::opts_chunk$set(echo = TRUE)
    library(knitr)
    library(xtable)
    library(pander)
    df <- mtcars[1:4,1:4]
    ```
    
    # Print tables using `kable`
    ```{r, 'kable'}
    kable(df)
    ```
    
    # Print tables using `xtable`
    ```{r, 'xtable', results='asis'}
    print(xtable(df, caption="My Table"))
    ```
    
    # Print tables using `pander`
    ```{r, 'pander'}
    pander(df)
    ```

[![enter image description here][2]][2]

**How can I stop xtable printing the comment ahead of each table?**

`options(xtable.comment = FALSE)`


  [1]: http://i.stack.imgur.com/FzRA5m.png
  [2]: http://i.stack.imgur.com/Fo8vzm.png

## Including LaTeX Preample Commands 
There are two possible ways of including LaTeX preamble commands (e.g. `\usepackage`) in a RMarkdown document.


  **1. Using the YAML option `header-includes`:**

    ---
    title: "Including LaTeX Preample Commands in RMarkdown"
    header-includes: 
       - \renewcommand{\familydefault}{cmss}
       - \usepackage[cm, slantedGreek]{sfmath}
       - \usepackage[T1]{fontenc}
    output: pdf_document
    ---
    
    ```{r setup, include=FALSE}
    knitr::opts_chunk$set(echo = TRUE, external=T)
    ```
    
    # Section 1
    
    As you can see, this text uses the Computer Moden Font!

[![enter image description here][1]][1]

  **2. Including External Commands with `includes`, `in_header`**


    ---
    title: "Including LaTeX Preample Commands in RMarkdown"
    output: 
      pdf_document:
        includes:
          in_header: includes.tex
    ---
    
    ```{r setup, include=FALSE}
    knitr::opts_chunk$set(echo = TRUE, external=T)
    ```
    
    # Section 1
    
    As you can see, this text uses the Computer Modern Font!

Here, the content of includes.tex are the same three commands we included with `header-includes`.


  **Writing a whole new template**

A possible third option is to write your own LaTex template and include it with `template`. But this covers a lot more of the structure than only the preamble.

    ---
    title: "My Template"
    author: "Martin Schmelzer"
    output:
      pdf_document:
        template: myTemplate.tex
    ---

  [1]: http://i.stack.imgur.com/U4eqOm.png

## Basic R-markdown document structure


