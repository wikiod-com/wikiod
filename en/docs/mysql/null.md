---
title: "NULL"
slug: "null"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Uses for NULL
* Data not yet known - such as `end_date`, `rating`
* Optional data - such as `middle_initial` (though that might be better as the empty string)
* 0/0 - The result of certain computations, such as zero divided by zero.
* NULL is not equal to "" (blank string) or 0 (in case of integer). 
* others?


## Testing NULLs
* `IS NULL` / `IS NOT NULL` -- `= NULL` does not work like you expect.
* `x <=> y` is a "null-safe" comparison.

In a `LEFT JOIN` tests for rows of `a` for which there is _not_ a corresponding row in `b`.

    SELECT ...
        FROM a
        LEFT JOIN b ON ...
        WHERE b.id IS NULL



