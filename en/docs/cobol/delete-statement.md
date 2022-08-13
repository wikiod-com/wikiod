---
title: "DELETE statement"
slug: "delete-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The `DELETE` statement deletes records from mass storage.  Some compilers allow the `DELETE` statement to be used with a `FILE` clause, to delete `FD` names (along with any associated indexing structures that may be required by the database management engine in use).

[![COBOL DELETE statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/oINXC.png

## Delete a record, key in primary key field
       identification division.
       program-id. deleting.

       environment division.
       configuration section.

       input-output section.
       file-control.
           select optional indexed-file
           assign to "indexed-file.dat"
           status is indexing-status
           organization is indexed
           access mode is dynamic
           record key is keyfield
           alternate record key is altkey with duplicates
           .

       ...

       procedure division.

       move "abcdef" to keyfield

       *> Delete a record by index
       delete indexed-file record
          invalid key
              display "No delete of " keyfield end-display
          not invalid key
              display "Record " keyfield " removed" end-display
       end-delete

       perform check-delete-status

       ...

