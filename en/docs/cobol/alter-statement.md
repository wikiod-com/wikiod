---
title: "ALTER statement"
slug: "alter-statement"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

The much beloved ALTER statement.  Changes the target of a GO TO paragraph.

No longer part of the COBOL standard, still supported by many compilers for reasons of backward compatibility. *(The syntax diagram is dimmed to show that this is no longer standard COBOL).* 

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ytIYo.png

## A contrived example using ALTER
     identification division.
     program-id. altering.
     date-written. 2015-10-28/06:36-0400.
     remarks. Demonstrate ALTER.
    
     procedure division.
     main section.
    
    *> And now for some altering.
     contrived.
     ALTER story TO PROCEED TO beginning
     GO TO story
     .
    
    *> Jump to a part of the story
     story.
     GO.
     .
    
    *> the first part
     beginning.
     ALTER story TO PROCEED to middle
     DISPLAY "This is the start of a changing story"
     GO TO story
     .
    
    *> the middle bit
     middle.
     ALTER story TO PROCEED to ending
     DISPLAY "The story progresses"
     GO TO story
     .
    
    *> the climatic finish
     ending.
     DISPLAY "The story ends, happily ever after"
     .
    
    *> fall through to the exit
     exit program.

With a run sample of

    prompt$ cobc -xj -debug altering.cob
    This is the start of a changing story
    The story progresses
    The story ends, happily ever after
    
    prompt$ COB_SET_TRACE=Y ./altering
    Source:     'altering.cob'
    Program-Id: altering         Entry:     altering               Line: 8
    Program-Id: altering         Section:   main                   Line: 8
    Program-Id: altering         Paragraph: contrived              Line: 11
    Program-Id: altering         Statement: ALTER                  Line: 12
    Program-Id: altering         Statement: GO TO                  Line: 13
    Program-Id: altering         Paragraph: story                  Line: 17
    Program-Id: altering         Paragraph: beginning              Line: 22
    Program-Id: altering         Statement: ALTER                  Line: 23
    Program-Id: altering         Statement: DISPLAY                Line: 24
    This is the start of a changing story
    Program-Id: altering         Statement: GO TO                  Line: 25
    Program-Id: altering         Paragraph: story                  Line: 17
    Program-Id: altering         Paragraph: middle                 Line: 29
    Program-Id: altering         Statement: ALTER                  Line: 30
    Program-Id: altering         Statement: DISPLAY                Line: 31
    The story progresses
    Program-Id: altering         Statement: GO TO                  Line: 32
    Program-Id: altering         Paragraph: story                  Line: 17
    Program-Id: altering         Paragraph: ending                 Line: 36
    Program-Id: altering         Statement: DISPLAY                Line: 37
    The story ends, happily ever after
    Program-Id: altering         Statement: EXIT PROGRAM           Line: 41
    Program-Id: altering         Exit:      altering
    prompt$

See http://open-cobol.sourceforge.net/faq/index.html#alter for more details.

