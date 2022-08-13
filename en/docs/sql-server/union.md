---
title: "UNION"
slug: "union"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Union and union all
**Union** operation combines the results of two or more queries into a single result set that includes all the rows that belong to all queries in the union and will ignore any duplicates that exist. **Union all** also does the same thing but include even the duplicate values. The concept of union operation will be clear from the example below.
Few things to consider while using union are:

1.The number and the order of the columns must be the same in all queries.

2.The data types must be compatible.

Example:

We have three tables : Marksheet1, Marksheet2 and Marksheet3. Marksheet3 is the duplicate table of Marksheet2 which contains same values as that of Marksheet2.

**Table1**: Marksheet1

[![enter image description here][1]][1]

**Table2**: Marksheet2

[![enter image description here][2]][2]

**Table3**: Marksheet3

[![enter image description here][3]][3]



**Union on tables Marksheet1 and Marksheet2**

    SELECT SubjectCode, SubjectName, MarksObtained 
    FROM Marksheet1
    UNION 
    SELECT CourseCode, CourseName, MarksObtained 
    FROM Marksheet2

**Note:** The output for union of the three tables will also be same as union on Marksheet1 and Marksheet2 because union operation does not take duplicate values. 
    
    SELECT SubjectCode, SubjectName, MarksObtained 
    FROM Marksheet1
    UNION 
    SELECT CourseCode, CourseName, MarksObtained 
    FROM Marksheet2   
    UNION
    SELECT SubjectCode, SubjectName, MarksObtained 
    FROM Marksheet3

**OUTPUT**

[![enter image description here][4]][4]


**Union All**

        
    SELECT SubjectCode, SubjectName, MarksObtained 
    FROM Marksheet1
    UNION ALL 
    SELECT CourseCode, CourseName, MarksObtained 
    FROM Marksheet2
    UNION ALL
    SELECT SubjectCode, SubjectName, MarksObtained 
    FROM Marksheet3

**OUTPUT**

[![enter image description here][5]][5]

You will notice here that the duplicate values from Marksheet3 are also displayed using union all.

  [1]: http://i.stack.imgur.com/N3RLb.png
  [2]: http://i.stack.imgur.com/WHDsX.png
  [3]: http://i.stack.imgur.com/ES51c.png
  [4]: http://i.stack.imgur.com/2P9zv.png
  [5]: http://i.stack.imgur.com/luvTc.png

