---
title: "data step"
slug: "data-step"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## getting data with data setp
    data newclass(keep=first_name sex weight yearborn);
      set sashelp.class(drop=height rename=(name=first_name));
      yearborn=year(date())-age;
      if yearborn >2002;
    run;

Data specifies the target data set. Keep option specifies columns to print to target.

Set specifies source data set. Drop specifies columns not to take. Rename renames name to first_name.

Yearborn is a calculated implicit numeric variable (column).

Filter and implicit output data with `if` for pupils born after 2002.
    

