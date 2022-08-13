---
title: "INSPECT statement"
slug: "inspect-statement"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The `INSPECT` statement is a scan and replace verb in COBOL.

[![INSPECT statement syntac diagram][1]][1]


Where `tallying-phrase` is:

[![tallying-phrase][2]][2]

`replacing-phrase` is:

    missing image

`before-after-phrase` is:

[![before-after-phrase][3]][3]


  [1]: http://i.stack.imgur.com/DAq5A.png
  [2]: http://i.stack.imgur.com/Axg4L.png
  [3]: http://i.stack.imgur.com/FLkhl.png

## INSPECT reformatting a date line
    GCobol identification division.
           program-id. inspecting.

           data division.
           working-storage section.
           01  ORIGINAL            pic XXXX/XX/XXBXX/XX/XXXXXXX/XX.
           01  DATEREC             pic XXXX/XX/XXBXX/XX/XXXXXXX/XX.

           procedure division.

           move function when-compiled to DATEREC ORIGINAL

           INSPECT DATEREC REPLACING ALL "/" BY ":" AFTER INITIAL SPACE

           display "Formatted function WHEN-COMPILED " ORIGINAL
           display " after INSPECT REPLACING         " DATEREC

           goback.
           end program inspecting.

Giving:

    Formatted function WHEN-COMPILED 2010/03/25 23/05/0900-04/00
     after INSPECT REPLACING         2010/03/25 23:05:0900-04:00


