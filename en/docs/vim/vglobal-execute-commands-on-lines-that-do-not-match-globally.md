---
title: "vglobal Execute commands on lines that do not match globally"
slug: "vglobal-execute-commands-on-lines-that-do-not-match-globally"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

:vglobal or :v is the opposite of :global or :g that operates on lines not matching the specified pattern (inverse).




## :v/pattern/d
Example:
    
    > cat example.txt
      TODO: complete this
      NOT this
      NOT that
      TODO: Complete that

Open the `example.txt` using `vim` and type `:v/TODO/d` in the `Ex` mode.  This will delete all lines that do not contain the `TODO` pattern.

[![example.txt][1]][1]
[![After deleting lines that don't match TODO][2]][2]


  [1]: https://i.stack.imgur.com/TEpu0.png
  [2]: https://i.stack.imgur.com/JtPQJ.png

