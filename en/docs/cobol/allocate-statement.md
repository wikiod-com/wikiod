---
title: "ALLOCATE statement"
slug: "allocate-statement"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Allocate working storage for a BASED item, or allocate a give size of heap storage.

See also: FREE statement
 
[![ALLOCATE syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/wulmr.png

## ALLOCATE statement
    01 pointer-var         usage POINTER.
    01 character-field     pic x(80) BASED value "Sample".

    ALLOCATE 1024 characters returning pointer-var
    ALLOCATE character-field
    ALLOCATE character-field INITIALIZED RETURNING pointer-var

See http://open-cobol.sourceforge.net/faq/index.html#allocate for more details.

