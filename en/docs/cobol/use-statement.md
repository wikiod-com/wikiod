---
title: "USE statement"
slug: "use-statement"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The `USE` statement specifies procedures to be used

- for error and exception handling in addition to those provided by other facilities
- before printing of a designated report group
- after detection of designated exception conditions

Obsolete usage includes specifying procedures to be used during `DEBUGGING`, and extensions include adding interstitial procedures for program start and end. 

[![USE statement syntax diagram][1]][1]


  [1]: https://i.stack.imgur.com/F35Xk.png

## USE statement with Report Writer DECLARATIVES
    035700 PROCEDURE DIVISION.
    035800
    035900 DECLARATIVES.
    036000
    036100 DEPT-HEAD-USE SECTION. USE BEFORE REPORTING DEPT-HEAD.
    036200 DEPT-HEAD-PROC.
    036300     SET DE-IX TO +1.
    036400     SEARCH DEPARTMENT-ENTRY
    036500         WHEN DE-NUMBER (DE-IX) = PRR-DEPARTMENT-NUMBER
    036600             MOVE ZEROS TO DE-GROSS (DE-IX), DE-FICA (DE-IX),
    036700                           DE-FWT (DE-IX), DE-MISC (DE-IX),
    036800                           DE-NET (DE-IX).
    036900
    037000 DEPT-HEAD-EXIT.
    037100     EXIT.
    037200
    037300 EMPL-FOOT-USE SECTION. USE BEFORE REPORTING EMPL-FOOT.
    037400 EMPL-FOOT-PROC.
    037500     MOVE PRR-EMPLOYEE-KEY TO WS-EMPLOYEE-KEY.
    037600
    037700 EMPL-FOOT-EXIT.
    037800     EXIT.
    037900
    038000 DEPT-FOOT-USE SECTION. USE BEFORE REPORTING DEPT-FOOT.
    038100 DEPT-FOOT-PROC.
    038200     MOVE DEPT-FOOT-GROSS TO DE-GROSS (DE-IX).
    038300     MOVE DEPT-FOOT-FICA TO DE-FICA (DE-IX).
    038400     MOVE DEPT-FOOT-FWT TO DE-FWT (DE-IX).
    038500     MOVE DEPT-FOOT-MISC TO DE-MISC (DE-IX).
    038600     MOVE DEPT-FOOT-NET TO DE-NET (DE-IX).
          *     SUPPRESS PRINTING.
    038700
    038800 DEPT-FOOT-EXIT.
    038900     EXIT.
    039000
    039100 COMP-FOOT-USE SECTION. USE BEFORE REPORTING COMP-FOOT.
    039200 COMP-FOOT-PROC.
    039300     PERFORM COMP-FOOT-CALC
    039400         VARYING WPCD-IX FROM +1 BY +1
    039500         UNTIL WPCD-IX > +6.
    039600     GO TO COMP-FOOT-EXIT.
    039700
    039800 COMP-FOOT-CALC.
    039900     SET DE-IX TO WPCD-IX.
    040000     SET WPCC-IX TO +1.
    040100     COMPUTE WPC-PERCENT (WPCD-IX WPCC-IX) ROUNDED =
    040200         ((DE-GROSS (DE-IX) / CO-GROSS) * 100) + .5.
    040300     SET WPCC-IX TO +2.
    040400     COMPUTE WPC-PERCENT (WPCD-IX WPCC-IX) ROUNDED =
    040500         ((DE-FICA (DE-IX) / CO-FICA) * 100) + .5.
    040600     SET WPCC-IX TO +3.
    040700     COMPUTE WPC-PERCENT (WPCD-IX WPCC-IX) ROUNDED =
    040800         ((DE-FWT (DE-IX) / CO-FWT) * 100) + .5.
    040900     SET WPCC-IX TO +4.
    041000     COMPUTE WPC-PERCENT (WPCD-IX WPCC-IX) ROUNDED =
    041100         ((DE-MISC (DE-IX) / CO-MISC) * 100) + .5.
    041200     SET WPCC-IX TO +5.
    041300     COMPUTE WPC-PERCENT (WPCD-IX WPCC-IX) ROUNDED =
    041400         ((DE-NET (DE-IX) / CO-NET) * 100) + .5.
    041500
    041600 COMP-FOOT-EXIT.
    041700     EXIT.
    041800
    041900 END DECLARATIVES.



