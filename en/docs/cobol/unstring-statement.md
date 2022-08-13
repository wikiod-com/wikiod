---
title: "UNSTRING statement"
slug: "unstring-statement"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The `UNSTRING` statement separates a sending field and places results in multiple receiving fields.

[![UNSTRING statement syntax diagram][1]][1]


  [1]: https://i.stack.imgur.com/Zg3vn.png

## UNSTRING example
    UNSTRING Input-Address
        DELIMITED BY "," OR "/"
        INTO
            Street-Address DELIMITER D1 COUNT C1
            Apt-Number DELIMITER D2 COUNT C2
            City DELIMITER D3 COUNT C3
            State DELIMITER D4 COUNT C4
            Zip-Code DELIMITER D5 COUNT C5
        WITH POINTER ptr-1
        ON OVERFLOW
            SET more-fields TO TRUE
    END-UNSTRING


