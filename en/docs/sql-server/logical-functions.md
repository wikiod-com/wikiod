---
title: "Logical Functions"
slug: "logical-functions"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## CHOOSE
<!-- if version [gte SQL Server 2012] -->

Returns the item at the specified index from a list of values. If `index` exceeds the bounds of `values` then `NULL` is returned.

Parameters:

1. `index`: integer, index to item in `values`. 1-based.
1. `values`: any type, comma separated list


    SELECT CHOOSE (1, 'apples', 'pears', 'oranges', 'bananas') AS chosen_result

    chosen_result
    -------------
    apples

<!-- end version if -->

## IIF
<!-- if version [gte SQL Server 2012] -->

Returns one of two values, depending on whether a given Boolean expression evaluates to true or false.

Parameters:

1. `boolean_expression` evaluated to dtermine what value to return
1. `true_value` returned if `boolean_expression` evaluates to true
1. `false_value` returned if `boolean_expression` evaluates to false


    SELECT IIF (42 > 23, 'I knew that!', 'That is not true.') AS iif_result

    iif_result
    ------------
    I knew that!

<!-- end version if -->

<!-- if version [lt SQL Server 2012] -->

`IIF` may be replaced by a `CASE` statement. The above example my be written as

    SELECT CASE WHEN 42 > 23 THEN 'I knew that!' ELSE 'That is not true.' END AS iif_result

    iif_result
    ------------
    I knew that!

<!-- end version if -->







