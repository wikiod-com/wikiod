---
title: "Cube Operation"
slug: "cube-operation"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Basic CUBE
Considering the following case:

We have user event data with 4 dimensions:
1. A/B Test bucket (prod/test)
2. client Type (web/mobile)
3. module (order/report)
4. event (click/view)


    test    mobile    order_module    click
    prod    web    order_module    view
    prod    mobile    order_module    click

A report system might want to report metrics for different combination, such as:
1. what's the total **click** count from **mobile** user?
1. what's the total **click** count from different test bucket?
1. what's the total **web** page **view** for **order_module** in **prod** bucket?

As you can see there are many combinations. It's not quite time efficient if we only store smallest granularity metrics and then roll them up when receiving a query. So one solution is to **pre-compute ALL combinations**.

Here's how we could do that with PIG's CUBE operation.

    example = LOAD './cube.example' AS (product:chararray, client:chararray, module:chararray, action:chararray);
    
    cubed_data = CUBE example BY CUBE(product, client, module, action);
    
    final_data = FOREACH cubed_data GENERATE $0, COUNT_STAR($1);
    
    dump final_data;

It will produce output of all combinations and total counts. See the output of previous dump -- with this stats, we could answer previous questions with direct answer. No further aggregation needed.

    ((prod,web,order_module,view),1)
    ((prod,web,order_module,),1)
    ((prod,web,,view),1)
    ((prod,web,,),1)
    ((prod,mobile,order_module,click),1)
    ((prod,mobile,order_module,),1)
    ((prod,mobile,,click),1)
    ((prod,mobile,,),1)
    ((prod,,order_module,view),1)
    ((prod,,order_module,click),1)
    ((prod,,order_module,),2)
    ((prod,,,view),1)
    ((prod,,,click),1)
    ((prod,,,),2)
    ((test,mobile,order_module,click),1)
    ((test,mobile,order_module,),1)
    ((test,mobile,,click),1)
    ((test,mobile,,),1)
    ((test,,order_module,click),1)
    ((test,,order_module,),1)
    ((test,,,click),1)
    ((test,,,),1)
    ((,web,order_module,view),1)
    ((,web,order_module,),1)
    ((,web,,view),1)
    ((,web,,),1)
    ((,mobile,order_module,click),2)
    ((,mobile,order_module,),2)
    ((,mobile,,click),2)
    ((,mobile,,),2)
    ((,,order_module,view),1)
    ((,,order_module,click),2)
    ((,,order_module,),3)
    ((,,,view),1)
    ((,,,click),2)
    ((,,,),3)

