---
title: "START statement"
slug: "start-statement"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The `START` statement provides a way to position a read in a file for subsequent sequential retrieval (by key).

[![START statement syntax diagram][1]][1]


The key relational can include (but is not limited to):

- KEY IS GREATER THAN
- KEY IS >
- KEY IS LESS THAN
- KEY IS <
- KEY IS EQUAL TO
- KEY IS =

- KEY IS NOT GREATER THAN
- KEY IS NOT >
- KEY IS NOT LESS THAN
- KEY IS NOT <
- KEY IS NOT EQUAL TO
- KEY IS NOT =
 
- KEY IS <>
- KEY IS GREATER THAN OR EQUAL TO
- KEY IS >=
- KEY IS LESS THAN OR EQUAL TO
- KEY IS <=

  [1]: https://i.stack.imgur.com/frfm5.png


## START example
    start indexing
       key is less than
           keyfield of indexing-record
       invalid key
           display "bad start: " keyfield of indexing-record
           set no-more-records to true
       not invalid key
           read indexing previous record
               at end set no-more-records to true
           end-read
    end-start


