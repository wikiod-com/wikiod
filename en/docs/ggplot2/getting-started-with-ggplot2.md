---
title: "Getting started with ggplot2"
slug: "getting-started-with-ggplot2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## How to install and run ggplot2
To install and load the current stable version of `ggplot2` for your R installation use:

    # install from CRAN
    install.packages("ggplot2")
    
To install the development version from github use

    # install.packages("devtools")
    devtools::install_github("hadley/ggplot2")

Load into your current R session, and make an example.



## Basic example of ggplot2
We show a plot similar to the showed at  https://www.wikiod.com/r/linear-models-regression#Linear regression on the mtcars dataset First with defaults and the with some customization of the parameters.

    
    #help("mtcars")
    fit <- lm(mpg ~ wt, data = mtcars)
    bs <- round(coef(fit), 3) 
    lmlab <- paste0("mpg = ", bs[1],
                    ifelse(sign(bs[2])==1, " + ", " - "), abs(bs[2]), " wt ")
    #range(mtcars$wt)
    library("ggplot2")

    #with defaults
    ggplot(aes(x=wt, y=mpg), data = mtcars) +  
      geom_point() +
      geom_smooth(method = "lm", se=FALSE, formula = y ~ x)

[![enter image description here][1]][1]

    #some customizations
    ggplot(aes(x=wt, y=mpg,colour="mpg"), data = mtcars) +  
    geom_point(shape=21,size=4,fill = "blue",alpha=0.55, color="red") +
    scale_x_continuous(breaks=seq(0,6, by=.5)) +
    geom_smooth(method = "lm", se=FALSE, color="darkgreen", formula = y ~ x) +
    geom_hline(yintercept=mean(mtcars$mpg), size=0.4, color="magenta") +
    xlab("Weight (1000 lbs)") + ylab("Miles/(US) gallon") +
    labs(title='Linear Regression Example',
         subtitle=lmlab,
         caption="Source: mtcars") +
    annotate("text", x = 4.5, y = 21, label = "Mean of mpg") +
    annotate("text", x = 4.8, y = 12, label = "Linear adjustment",color = "red") +
    theme_bw() 

[![enter image description here][2]][2]

See other examples at https://www.wikiod.com/r/ggplot2


  [1]: http://i.stack.imgur.com/Fyc0l.png
  [2]: https://i.stack.imgur.com/HviT3.jpg

