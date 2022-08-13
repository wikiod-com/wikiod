---
title: "PERFORM statement"
slug: "perform-statement"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

The PERFORM statement transfers control to one or more procedures and returns control implicitly when the sequence completes.  PERFORM can also be used for inline loops withing the scope of the PERFORM.

The `VARYING` phrase allows for nesting with one or more `AFTER` clauses, and the conditional test can be `BEFORE` (default) or `AFTER` each loop.

The `THRU` clause of a procedural perform assumes sequential top down control flow from `procedure-1` through the end of `procedure-2`.  *THRU* is a contentious issue, and many programmers prefer `PERFORM` by `SECTION` rather than using `THRU` paragraphs.  Some shops may mandate `PERFORM THRU` with an explicit exit point paragraph, others may ban the use of `THRU` finding it more difficult to debug.  

Procedural perform:

[![PERFORM procedure syntax diagram][1]][1]


Inline perform:

[![Inline PERFORM syntax diagram][2]][2]

Where `varying-phrase` is:

[![varying-phrase][3]][3]


  [1]: http://i.stack.imgur.com/bniXN.png
  [2]: http://i.stack.imgur.com/jq7le.png
  [3]: http://i.stack.imgur.com/323kR.png

## Inline PERFORM VARYING
    PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 5
        DISPLAY TALLY
    END-PERFORM

## Procedural PERFORM
    PERFORM some-paragraph

