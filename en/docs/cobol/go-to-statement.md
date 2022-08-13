---
title: "GO TO statement"
slug: "go-to-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The much beloved `GO TO`.  COBOL includes named paragraphs and sections, along with other labels, and any of them can be the target of a `GO` statement.
[![GO statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/CcCO7.png

## GO statement
    GO TO label

    GO TO label-1 label-2 label-3 DEPENDING ON identifier-1

    GO TO label OF section

    GO.

The last line example indicates that an `ALTER` statement is in play, and another part of the code will specify which actual `label` is the target of the jump.

