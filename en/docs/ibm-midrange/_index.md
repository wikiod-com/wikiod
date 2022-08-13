---
title : ibm-midrange Tutorial
slug : ibm-midrange-tutorial
weight : 9984
draft : false
images : []
type : docs
---

IBM midrange is a generic term which encompasses a family of 'baby mainframe' computers which IBM have been making since the late 1960s.

 - 1969: [System/3][1].  System/3 is not compatible with IBM's System/360, and it costs about half as much.  The primary programming language is RPG II.
 - 1975: [System/32][2] is announced.  
 - 1977: [System/34][3] is announced along with a new family of 'dumb' terminals: the 5250 series.
 - 1978: [System/38][4] was announced.  System/38 was intended to replace System/3, and had a radically different architecture.  
 - 1983: [System/36][5] is announced as a follow on to System/34.
 - 1988: [AS/400][6] is announced as a follow on to both System/38 and System/36.  It runs System/38 code natively, and has an emulator for System/36 code.
 - 2000: eServer iSeries is announced.  For a brief history of the AS/400 to iSeries transition, see [A Brief history of AS/400][7].
 - 2008: Power Systems is announced.  This is a merger of the iSeries and pSeries lines into a single system.

While System/3, /32, /34, /36 all had flat, non-relational file systems (much like a Windows PC), System/38, AS/400, iSeries, and Power all have a midrange version of DB2 built-in.  In addition to DB2, IBM i (the name of the operating system) also has a subset of AIX called PASE, a *nix-like shell called QShell, and Java built-in.  The primary programming language is ILE RPG, which allows for embedded SQL as well as 'native' record level access I/O.  Additionally, to retain compatibility with prior generations of the operating system, Rexx, CL (the native command scripting language), DDS (for describing database, and device files), and earlier versions of RPG are still supported.  IBM i 7.2 and up includes open source software like Python, Node.js, and GCC.


  [1]: http://www-03.ibm.com/ibm/history/exhibits/rochester/rochester_4008.html
  [2]: http://www-03.ibm.com/ibm/history/exhibits/rochester/rochester_4017.html
  [3]: http://www-03.ibm.com/ibm/history/exhibits/vintage/vintage_4506VV2236.html
  [4]: http://www-03.ibm.com/ibm/history/exhibits/rochester/rochester_4009.html
  [5]: http://www-03.ibm.com/ibm/history/exhibits/rochester/rochester_4018.html
  [6]: http://www-03.ibm.com/ibm/history/exhibits/rochester/rochester_4010.html
  [7]: http://www-03.ibm.com/ibm/history/documents/pdf/as400.pdf

