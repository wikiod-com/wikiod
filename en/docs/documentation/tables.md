---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Linebreaks
## Automatic Linebreaks ##

Table content is line wrapped automatically if it is too wide.
In the following example, text is added to the second column, which causes oth the text itself and the column header of the first column to be wrapped.


    | this is a really wide column header | another header |
    | --- | --- |
    | | |


| this is a really wide column header | another header |
| --- | --- |
| | |

    | this is a really wide column header | another header |
    | --- | --- |
    | | Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magn |

| this is a really wide column header | another header |
| --- | --- |
| | Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magn |

## Manual Linebreaks ##

It's also possible to insert manual breaks as you would in regular text.
This can allow other columns to expand even further. 

    | this <br/> is <br/>  a <br/> really <br/> wide <br/> column <br/> header | another header |
    | --- | --- |
    | | Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magn |


| this <br/> is <br/>  a <br/> really <br/> wide <br/> column <br/> header | another header |
| --- | --- |
| | Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magn |

