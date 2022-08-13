---
title: "Customizing axes, titles, and legends"
slug: "customizing-axes-titles-and-legends"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

In this topic, we'll look to explain how to Customise axes, titles and legends whilst using the `ggplot2` library.

## Change legend title and increase keysize
    # load the library
    library(ggplot2)
    
    # create a blank canvas
    g <- ggplot(data = diamonds)

    g + geom_bar(aes(x = cut, fill = cut)) + 
        scale_fill_discrete(guide = guide_legend(title = "CUT", 
                                                 keywidth = 2, 
                                                 keyheight = 2))
[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/cRPmB.jpg
  


## Compare frequencies across groups and remove legend title
    g + geom_bar(aes(x = cut, fill = color), position = "fill") + 
        guides(fill = guide_legend(title = NULL))
[![enter image description here][2]][2]

[2]: http://i.stack.imgur.com/XZVxl.jpg



## Place overlapping objects next to each other and change colours of axes texts
    g + geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") +
        theme(axis.text = element_text(colour = "red", size = 12))
[![enter image description here][3]][3]

  [3]: http://i.stack.imgur.com/JLIx7.jpg

## Fine tuning axes ticks, texts, and titles
     g + geom_histogram(aes(price, fill = cut), binwidth = 500) + 
        labs(x = "Price", y = "Number of diamonds", 
             title = "Distribution of prices \n across Cuts") + 
        theme(plot.title = element_text(colour = "red", face = "italic"),
            axis.title.x = element_text(face="bold", 
                                          colour="darkgreen", size = 12),
          axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 12), 
          axis.title.y = element_text(face="bold", 
                                      colour="darkblue", size = 12),
          axis.text.y  = element_text(size = 12, colour = "brown"))


[![enter image description here][4]][4]


  [4]: http://i.stack.imgur.com/H0vC1.jpg


