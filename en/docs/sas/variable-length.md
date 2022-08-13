---
title: "Variable Length"
slug: "variable-length"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- LENGTH variable(s) <$>length;

## Parameters
|Parameter|Details|
|---------|-------|
|variable(s)|variable(s) you wish to assign a length to|
|$|optional parameter that specifies if your variable is a character variable|
|length|integer that specifies the length of the variable|

## Assigning length to a character variable
    data table;
    set table;
    length state_full $8;
    if state = 'KS' then state_full = 'Kansas';
    else if state = 'CO' then state_full = 'Colorado';
    else state_full = 'Other';
    run;

