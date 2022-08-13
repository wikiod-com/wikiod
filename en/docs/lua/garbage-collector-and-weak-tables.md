---
title: "Garbage collector and weak tables"
slug: "garbage-collector-and-weak-tables"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
1. collectgarbage(gcrule [, gcdata]) -- collect garbage using gcrule
2. setmetatable(tab, {__mode = weakmode}) -- set weak mode of tab to weakmode

## Parameters
| parameter | details |
|----------|---------|
|gcrule & gcdata| Action to gc (garbage collector): `"stop"` (stop collecting), `"restart"` (start collecting again), `"collect"` or `nil` (collect all garbage), `"step"` (do one collecting step), `"count"` (return count of used memory in KBs), `"setpause"` and data is number from `0`% to `100`% (set pause parameter of gc), `"setstepmul"` and data is number from `0`% to `100` (set `"stepmul"` for gc).|
| weakmode | Type of weak table: `"k"` (only weak keys), `"v"` (only weak values), `"vk"` (weak keys and values) |

## Weak tables
    local t1, t2, t3, t4 = {}, {}, {}, {} -- Create 4 tables
    local maintab = {t1, t2} -- Regular table, strong references to t1 and t2
    local weaktab = setmetatable({t1, t2, t3, t4}, {__mode = 'v'}) -- table with weak references.

    t1, t2, t3, t4 = nil, nil, nil, nil -- No more "strong" references to t3 and t4
    print(#maintab, #weaktab) --> 2 4

    collectgarbage() -- Destroy t3 and t4 and delete weak links to them.
    print(#maintab, #weaktab) --> 2 2

