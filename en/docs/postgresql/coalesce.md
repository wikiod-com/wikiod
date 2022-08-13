---
title: "COALESCE"
slug: "coalesce"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Coalesce returns the first none null argument from a set of arguments. 
Only the first non null argument is return, all subsequent arguments are ignored. 
The function will evaluate to null if all arguments are null. 



    
    



## Single non null argument
    PGSQL> SELECT COALESCE(NULL, NULL, 'HELLO WORLD');

    coalesce
    --------
    'HELLO WORLD'


   

## Multiple non null arguments
 PGSQL> SELECT COALESCE(NULL, NULL, 'first non null', null, null, 'second non null');
    
    coalesce
    --------
    'first non null'

## All null arguments
    PGSQL> SELECT COALESCE(NULL, NULL, NULL);
    
    coalesce
    --------
    

