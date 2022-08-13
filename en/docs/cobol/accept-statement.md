---
title: "ACCEPT statement"
slug: "accept-statement"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The COBOL ACCEPT statement is used to retrieve data from the system.
 

[![ACCEPT statement][1]][1]


  [1]: http://i.stack.imgur.com/DdpJO.png

## ACCEPT statement
    ACCEPT variable.
    ACCEPT variable FROM CONSOLE.

    ACCEPT variable FROM ENVIRONMENT "path".
    ACCEPT variable FROM COMMAND-LINE.

    ACCEPT variable FROM ARGUMENT-NUMBER
    ACCEPT variable FROM ARGUMENT-VALUE

    ACCEPT variable AT 0101.
    ACCEPT screen-variable.

    ACCEPT today FROM DATE.
    ACCEPT today FROM DATE YYYYMMDD.
    ACCEPT thetime FROM TIME.

    ACCEPT theday FROM DAY.
    ACCEPT theday FROM DAY YYYYDDD.

    ACCEPT weekday FROM DAY-OF-WEEK.

    ACCEPT thekey FROM ESCAPE KEY.

    ACCEPT username FROM USER NAME.

    ACCEPT exception-stat FROM EXCEPTION STATUS.

    ACCEPT some-data FROM device-name.

See http://open-cobol.sourceforge.net/faq/index.html#accept for more details.

