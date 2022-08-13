---
title: "Recursive queries"
slug: "recursive-queries"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

There are no real recursive querys!

## Sum of Integers
    WITH RECURSIVE t(n) AS (
        VALUES (1)
      UNION ALL
        SELECT n+1 FROM t WHERE n < 100
    )
    SELECT sum(n) FROM t;

[Link to Documentation][1]


  [1]: https://www.postgresql.org/docs/9.6/static/queries-with.html

