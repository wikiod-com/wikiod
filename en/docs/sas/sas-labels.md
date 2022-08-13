---
title: "SAS Labels"
slug: "sas-labels"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Labels can be used to describe a variable which helps improve the readability of your outputs. Labels can be permanently created in the `DATA` step or temporarily created in a `PROC` step. 

## Create Permanent Variable Labels in DATA step
    data table;
        set table;
        label variable1 = 'label1'
                variable2 = 'label2'
                variable3 = 'label3';
    run;

