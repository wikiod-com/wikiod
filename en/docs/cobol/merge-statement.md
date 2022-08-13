---
title: "MERGE statement"
slug: "merge-statement"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The MERGE statement will merge one or more like formatted COBOL data files into a single output file. The programmer can assume control over the`OUTPUT PROCEDURE`, which uses the `RELEASE` statement, or use internal COBOL runtime mechanisms with the `GIVING` clause.

[![MERGE statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/7XZQJ.png

## MERGE regional data into master
    GCobol >>SOURCE FORMAT IS FIXED
          *> ***************************************************************
          *> Purpose:   Demonstrate a merge pass
          *> Tectonics: cobc -x gnucobol-merge-sample.cob
          *> ***************************************************************
           identification division.
           program-id. gnucobol-merge-sample.
    
           environment division.
           configuration section.
           repository.
               function all intrinsic.
    
    files  input-output section.
           file-control.
               select master-file
                   assign to "master-sample.dat"
                   organization is line sequential.
    
               select eastern-transaction-file
                   assign to "east-transact-sample.dat"
                   organization is line sequential.
    
               select western-transaction-file
                   assign to "west-transact-sample.dat"
                   organization is line sequential.
    
               select merged-transactions
                   assign to "merged-transactions.dat"
                   organization is line sequential.
    
               select working-merge
                   assign to "merge.tmp".
    
    data   data division.
           file section.
           fd master-file.
              01 master-record     pic x(64).
    
           fd eastern-transaction-file.
              01 transact-rec      pic x(64).
    
           fd western-transaction-file.
              01 transact-rec      pic x(64).
    
           fd merged-transactions.
              01 new-rec           pic x(64).
    
           sd working-merge.
              01 merge-rec.
                 02 master-key     pic 9(8).
                 02 filler         pic x.
                 02 action         pic xxx.
                 02 filler         PIC x(52).
    
          *> ***************************************************************
          *> not much code
          *>     trick.  DEP, CHQ, BAL are action keywords.  They sort
          *>     descending as DEP, CHQ, BAL, so main can do all deposits,
          *>     then all withdrawals, then balance reports, for each id.
          *> ***************************************************************
    code   procedure division.
           merge working-merge
               on ascending key master-key
                  descending key action
               using eastern-transaction-file,
                     western-transaction-file,
                     master-file
               giving merged-transactions
    done   goback.
           end program gnucobol-merge-sample.


