---
title: "CANCEL statement"
slug: "cancel-statement"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

The CANCEL statement ensures that a referenced program will be in an initial state the next time it is called, and to unload any resources for the module.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gqOCK.png

## CANCEL statement
    CALL "submodule"
    CALL "submodule"

    CANCEL "submodule"
    CALL "submodule"

Any static data in the working set of `submodule` will be in an initial state on the last `CALL` statement above.  The second `CALL` will have any initial values set as left overs from the first `CALL`.

COBOL compilers can support physical cancel (object unloaded from memory) and/or virtual cancel (ensure an initial state, but leave the object available to the host operating environment).  This is an implementation detail.

See http://open-cobol.sourceforge.net/faq/index.html#cancel for more details.

