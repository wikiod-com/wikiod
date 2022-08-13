---
title: "Predefined Macros"
slug: "predefined-macros"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

ANSI C defines a number of macros. Although each one is available for your use in programming, the predefined macros should not be directly modified.

## Syntax

 1. __DATE__    The current date as a character literal in "MMM DD YYYY" format
 2. __TIME__    The current time as a character literal in "HH:MM:SS" format
 3. __FILE__    This contains the current filename as a string literal.
 4. __LINE__    This contains the current line number as a decimal constant.
 5. __STDC__    Defined as 1 when the compiler complies with the ANSI standard.

## Predefined Macros
    #import <Foundation/Foundation.h>
    
    int main()
    {
       NSLog(@"File :%s\n", __FILE__ );
       NSLog(@"Date :%s\n", __DATE__ );
       NSLog(@"Time :%s\n", __TIME__ );
       NSLog(@"Line :%d\n", __LINE__ );
       NSLog(@"ANSI :%d\n", __STDC__ );
       
       return 0;
    }

**When the above code in a file main.m is compiled and executed, it produces the following result:**


    2013-09-14 04:46:14.859 demo[20683] File :main.m
    2013-09-14 04:46:14.859 demo[20683] Date :Sep 14 2013
    2013-09-14 04:46:14.859 demo[20683] Time :04:46:14
    2013-09-14 04:46:14.859 demo[20683] Line :8
    2013-09-14 04:46:14.859 demo[20683] ANSI :1

