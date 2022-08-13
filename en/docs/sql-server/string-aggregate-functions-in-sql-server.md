---
title: "String Aggregate functions in SQL Server"
slug: "string-aggregate-functions-in-sql-server"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using STUFF for string aggregation
We have a Student table with SubjectId. Here the requirement is to concatenate based on subjectId. 

All SQL Server versions

    create table #yourstudent (subjectid int, studentname varchar(10))

    insert into #yourstudent (subjectid, studentname) values
     ( 1       ,'Mary'    )
    ,( 1       ,'John'    )
    ,( 1       ,'Sam'    )
    ,( 2       ,'Alaina')
    ,( 2       ,'Edward')

    select subjectid,  stuff(( select concat( ',', studentname) from #yourstudent y where y.subjectid = u.subjectid for xml path('')),1,1, '') 
        from #yourstudent u
        group by subjectid



## String_Agg for String Aggregation
In case of SQL Server 2017 or vnext we can use in-built STRING_AGG for this aggregation. For same student table,

    create table #yourstudent (subjectid int, studentname varchar(10))

    insert into #yourstudent (subjectid, studentname) values
     ( 1       ,'Mary'    )
    ,( 1       ,'John'    )
    ,( 1       ,'Sam'    )
    ,( 2       ,'Alaina')
    ,( 2       ,'Edward')

    select subjectid, string_agg(studentname, ',') from #yourstudent
        group by subjectid


