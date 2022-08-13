---
title: "DO Loop"
slug: "do-loop"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## DO Loop
    DATA salary;
        /*define variables*/
        raise=0.1;
        salary=50000;
        year=1;
        /*do loop*/
        DO year=1 to 20 by 2;
            salary + salary*raise;
            output; /*generates an observation for each iteration of the do loop, optional*/
        END;
    RUN;

## Macro do loop
    %macro doloop;
      %do age=11 %to 15 %by 2;
        title Age=&age.;
        proc print data=sashelp.class(where=(age=&age.));
        run;
      %end;
    %mend;
    %doloop;

