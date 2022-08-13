---
title: "Performance Tuning"
slug: "performance-tuning"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
-    Don't use DISTINCT and GROUP BY in the same SELECT.

-    Don't paginate via OFFSET, "remember where you left off".

-    WHERE (a,b) = (22,33) does not optimize at all.

-    Explicitly say ALL or DISTINCT after UNION -- it reminds you pick between the faster ALL or the slower DISTINCT.

-    Don't use SELECT *, especially if you have TEXT or BLOB columns that you don't need. There is overhead in tmp tables and transmission.

-    It is faster when the GROUP BY and ORDER BY can have exactly the same list.

-    Don't use FORCE INDEX; it may help today, but will probably hurt tomorrow.



See also discussions about ORDER BY, LIKE, REGEXP, etc. Note: this needs editing with links and more Topics.

[_Cookbook on building optimal indexes_](https://mariadb.com/kb/en/mariadb/building-the-best-index-for-a-given-select/).



## Add the correct index
This is a huge topic, but it is also the most important "performance" issue.

The main lesson for a novice is to learn of "composite" indexes.  Here's a quick example:

    INDEX(last_name, first_name)

is excellent for these:

    WHERE last_name = '...'
    WHERE first_name = '...' AND last_name = '...'   -- (order in WHERE does not matter)

but not for

    WHERE first_name = '...'   -- order in INDEX _does_ matter
    WHERE last_name = '...' OR first_name = '...'   -- "OR" is a killer

## Don't hide in function
A common mistake is to hide an indexed column inside a function call.  For example, this can't be helped by an index:

    WHERE DATE(dt) = '2000-01-01'

Instead, given `INDEX(dt)` then these may use the index:

    WHERE dt = '2000-01-01'  -- if `dt` is datatype `DATE`

This works for `DATE`, `DATETIME`, `TIMESTAMP`, and even `DATETIME(6)` (microseconds):

    WHERE dt >= '2000-01-01'
      AND dt  < '2000-01-01' + INTERVAL 1 DAY



## OR


## Set the cache correctly
`innodb_buffer_pool_size` should be about 70% of available RAM.

## Negatives
Here are some things that are not likely to help performance.  They stem from out-of-date information and/or naivety.

* InnoDB has improved to the point where MyISAM is unlikely to be better.
* `PARTITIONing` rarely provides performance benefits; it can even hurt performance.
* Setting `query_cache_size` bigger than 100M will usually _hurt_ performance.
* Increasing lots of values in `my.cnf` may lead to 'swapping', which is a _serious_ performance problem.
* "Prefix indexes" (such as `INDEX(foo(20))`) are generally useless.
* `OPTIMIZE TABLE` is almost always useless.  (And it involves locking the table.)

## Have an INDEX


## Subqueries


## JOIN + GROUP BY


## Avoid inefficient constructs
    x IN ( SELECT ... )
turn into a `JOIN`

When possible, avoid `OR`.

Do not 'hide' an indexed column in a function, such as `WHERE DATE(x) = ...`; reformulate as `WHERE x = ...`

You can generally avoid `WHERE LCASE(name1) = LCASE(name2)` by having a suitable collation.

Do no use `OFFSET` for "pagination", instead 'remember where you left off'.

Avoid `SELECT * ...` (unless debugging).

_Note to Maria Deleva, Barranka, Batsu: This is a place holder; please make remove these items as you build full-scale examples.  After you have done the ones you can, I will move in to elaborate on the rest and/or toss them._

