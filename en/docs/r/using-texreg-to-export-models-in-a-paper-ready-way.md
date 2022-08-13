---
title: "Using texreg to export models in a paper-ready way"
slug: "using-texreg-to-export-models-in-a-paper-ready-way"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The texreg package helps to export a model (or several models) in a neat paper-ready way. The result may be exported as HTML or .doc (MS Office Word).

# Links

- [CRAN page][1]


  [1]: https://cran.r-project.org/package=texreg

## Printing linear regression results

    # models
    fit1 <- lm(mpg ~ wt, data = mtcars)
    fit2 <- lm(mpg ~ wt+hp, data = mtcars)
    fit3 <- lm(mpg ~ wt+hp+cyl, data = mtcars)
    
    # export to html
    texreg::htmlreg(list(fit1,fit2,fit3),file='models.html')
    
    
    # export to doc
    texreg::htmlreg(list(fit1,fit2,fit3),file='models.doc')

The result looks like a table in a paper.

[![enter image description here][1]][1]

***
There are several additional handy parameters in `texreg::htmlreg()` function. Here is a use case for the most helpful parameters.

    # export to html
    texreg::htmlreg(list(fit1,fit2,fit3),file='models.html',
                    single.row = T,
                    custom.model.names = LETTERS[1:3],
                    leading.zero = F,
                    digits = 3)

Which result in a table like this

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/M8GyJ.png
  [2]: http://i.stack.imgur.com/wMN7q.png

