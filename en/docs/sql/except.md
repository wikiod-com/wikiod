---
title: "EXCEPT"
slug: "except"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

`EXCEPT` returns any distinct values from the dataset to the left of the EXCEPT operator that are not also returned from the right dataset.

## Select dataset except where values are in this other dataset
    --dataset schemas must be identical
    SELECT 'Data1' as 'Column' UNION ALL
    SELECT 'Data2' as 'Column' UNION ALL
    SELECT 'Data3' as 'Column' UNION ALL
    SELECT 'Data4' as 'Column' UNION ALL
    SELECT 'Data5' as 'Column'
    EXCEPT
    SELECT 'Data3' as 'Column'
    --Returns Data1, Data2, Data4, and Data5

