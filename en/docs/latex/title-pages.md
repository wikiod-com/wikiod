---
title: "Title Pages"
slug: "title-pages"
draft: false
images: []
weight: 9973
type: docs
toc: true
---


`\title{<title>}`, `\author{<author>}` and `\date{<date}` internally store the content.


`\maketitle` produces a standard title page with the previously defined values.

## Standard report titlepage
<!-- language: lang-latex -->

    \documentclass{report}

    \begin{document}

    \title{I want to be a Wombat}
    \author{Carl Capybara}
    \maketitle

    \end{document}

This will create a title page with no other content:

[![Rendered result][1]][1]


  [1]: http://i.stack.imgur.com/7hogM.png

