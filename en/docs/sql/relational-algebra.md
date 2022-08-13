---
title: "Relational Algebra"
slug: "relational-algebra"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Overview
*Relational Algebra* is not a full-blown *SQL* language, but rather a way to gain theoretical understanding of relational processing.  As such it shouldn't make references to physical entities such as tables, records and fields; it should make references to abstract constructs such as relations, tuples and attributes.  Saying that, I won't use the academic terms in this document and will stick to the more widely known layman terms - tables, records and fields.

A couple of rules of relational algebra before we get started:

 - The operators used in relational algebra work on whole tables rather than individual records.
 - The result of a relational expression will always be a table (this is called the *closure property*)

Throughout this document I will be referring to the follow two tables:  

[![Example Tables][1]][1]

# SELECT #  
The **select** operator returns a subset of the main table.  
***select** < table > **where** < condition >*  

For example, examine the expression:  

**select** People **where** DepartmentID = 2  

This can be written as:  
[![enter image description here][2]][2]

This will result in table whose records comprises of all records in the *People* table where the *DepartmentID* value is equal to 2:  
[![enter image description here][3]][3]  

Conditions can also be joined to restrict the expression further:  

**select** People **where** StartYear > 2005 **and** DepartmentID = 2  

will result in the following table:  
[![enter image description here][4]][4]

# PROJECT #
The **project** operator will return distinct field values from a table.  
***project** < table > **over** < field list >*  

For example, examine the following expression:  
**project** People **over** StartYear  

This can be written as:  
[![enter image description here][5]][5]

This will result in a table comprising of the distinct values held within the *StartYear* field of the *People* table.  
[![enter image description here][6]][6]

Duplicate values are removed from the resulting table due to the *closure property* creating a relational table: all records in a relational table are required to be distinct.  

If the *field list* comprises more than a single field then the resulting table is a distinct version of these fields.  
**project** People **over** StartYear, DepartmentID will return:  
[![enter image description here][7]][7]  
One record is removed due to the duplication of 2006 *StartYear* and 1 *DepartmentID*.  

# GIVING #  
Relational expressions can be chained together by naming the individual expressions using the **giving** keyword, or by embedding one expression within another.  

*< relational algebra expression > **giving** < alias name >*  

For example, consider the following expressions:  
**select** People **where** DepartmentID = 2 **giving** A  
**project** A **over** PersonName **giving** B  

This will result in table B below, with table A being the result of the first expression.  
[![enter image description here][8]][8]  

The first expression is evaluated and the resulting table is given the alias A.  This table is then used within the second expression to give the final table with an alias of B.  

Another way of writing this expression is to replace the table alias name in the second expression with the entire text of the first expression enclosed within brackets:  
**project** (**select** People **where** DepartmentID = 2) **over** PersonName **giving** B  

This is called a *nested expression*.  

# NATURAL JOIN #
A natural join sticks two tables together using a common field shared between the tables.  

***join** < table 1 > **and** < table 2 > **where** < field 1 > = < field 2 >*  
assuming that < field 1 > is in < table 1 > and < field 2 > is in < table 2 >.

For example, the following join expression will join *People* and *Departments* based on the *DepartmentID* and *ID* columns in the respective tables:  
**join** People **and** Departments **where** DepartmentID = ID  

[![enter image description here][9]][9]

Note that only *DepartmentID* from the *People* table is shown and not *ID* from the *Department* table.  Only one of the fields being compared needs to be shown which is generally the field name from the first table in the join operation.

Although not shown in this example it is possible that joining tables may result in two fields having the same heading.  For example, if I had used the heading *Name* to identify the *PersonName* and *Dept* fields (i.e. to identify the Person Name and the Department Name).  When this situation arises we use the table name to qualify the field names using the dot notation: *People.Name* and *Departments.Name*  

**join** combined with **select** and **project** can be used together to pull information:  

***join** People **and** Departments **where** DepartmentID = ID **giving** A  
**select** A **where** StartYear = 2005 **and** Dept = 'Production' **giving** B  
**project** B **over** PersonName **giving** C*  

or as a combined expression:

***project** (**select** (**join** People **and** Departments **where** DepartmentID = ID) **where** StartYear = 2005 **and** Dept = 'Production') **over** PersonName **giving** C*  

This will result in this table:  
[![enter image description here][10]][10]

# ALIAS #

# DIVIDE #

# UNION #

# INTERSECTION #

# DIFFERENCE #

# UPDATE ( := ) #

# TIMES #

  [1]: http://i.stack.imgur.com/LA0hy.png
  [2]: http://i.stack.imgur.com/hx5nv.png
  [3]: http://i.stack.imgur.com/WHcPH.png
  [4]: http://i.stack.imgur.com/4EGZJ.png
  [5]: http://i.stack.imgur.com/BPUGJ.png
  [6]: http://i.stack.imgur.com/8DYph.png
  [7]: http://i.stack.imgur.com/arkvR.png
  [8]: http://i.stack.imgur.com/Y754U.png
  [9]: http://i.stack.imgur.com/my3Pr.png
  [10]: http://i.stack.imgur.com/a58aB.png

