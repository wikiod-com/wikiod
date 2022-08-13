---
title: "INITIALIZE statement"
slug: "initialize-statement"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The `INITIALIZE` statement sets selected data to specified values.

[![INITIALIZE statement syntax diagram][1]][1]


Where `category-name` is:

[![category-name clause][2]][2]


  [1]: http://i.stack.imgur.com/wciOg.png
  [2]: http://i.stack.imgur.com/bEu08.png

## Various INITIALIZE clauses
    01  fillertest.
        03 fillertest-1 PIC 9(10) value 2222222222.
        03 filler       PIC X     value '|'.
        03 fillertest-2 PIC X(10) value all 'A'.
        03 filler       PIC 9(03) value 111.
        03 filler       PIC X     value '.'.

    INITIALIZE fillertest

    INITIALIZE fillertest REPLACING NUMERIC BY 9

    INITIALIZE fillertest REPLACING ALPHANUMERIC BY 'X'

    INITIALIZE fillertest REPLACING ALPHANUMERIC BY ALL 'X'

    INITIALIZE fillertest WITH FILLER

    INITIALIZE fillertext ALL TO VALUE

Giving:

    fillertest on start:
    2222222222|AAAAAAAAAA111.
    fillertest after initialize:
    0000000000|          111.
    fillertest after initialize replacing numeric by 9:
    0000000009|          111.
    fillertest after initialize replacing alphanumeric by "X":
    0000000009|X         111.
    fillertest after initialize replacing alphanumeric by all "X":
    0000000009|XXXXXXXXXX111.
    fillertest after initialize with filler:
    0000000000           000
    fillertest after initialize all to value:
    2222222222|AAAAAAAAAA111.


