---
title: "DB2 Constraint Information"
slug: "db2-constraint-information"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This documentation will help anyone who is looking for all the Conrtraints on a column of a table. The query can be modified to find the table/columns based on the constraint name.

## Get constraints based on column name
    select cst.constraint_schema, cst.constraint_name,                 
           fk.table_name, fk.ordinal_position, fk.column_name,         
           pk.table_name, pk.column_name                               
      from qsys2.syscst cst join qsys2.syskeycst fk                   
          on fk.constraint_schema = cst.constraint_schema              
            and fk.constraint_name = cst.constraint_name               
        join qsys2.sysrefcst ref                                       
          on ref.constraint_schema = cst.constraint_schema             
            and ref.constraint_name = cst.constraint_name              
        join qsys2.syskeycst pk                                        
          on pk.constraint_schema = ref.unique_constraint_schema       
            and pk.constraint_name = ref.unique_constraint_name        
      where cst.constraint_type = 'FOREIGN KEY'                        
        and fk.ordinal_position = pk.ordinal_position                  
        and pk.table_name = 'PRIMARYTABLE'                             
        and pk.column_name = 'EMPID'                                   
    
    order by cst.constraint_schema, cst.constraint_name             

## Get constraints info based on constraint name
    select cst.constraint_schema, cst.constraint_name,                 
           fk.table_name, fk.ordinal_position, fk.column_name,         
           pk.table_name, pk.column_name                               
      from qsys2.syscst cst join qsys2.syskeycst fk                   
          on fk.constraint_schema = cst.constraint_schema              
            and fk.constraint_name = cst.constraint_name               
        join qsys2.sysrefcst ref                                       
          on ref.constraint_schema = cst.constraint_schema             
            and ref.constraint_name = cst.constraint_name              
        join qsys2.syskeycst pk                                        
          on pk.constraint_schema = ref.unique_constraint_schema       
            and pk.constraint_name = ref.unique_constraint_name        
      where fk.ordinal_position = pk.ordinal_position                  
        and  cst.constraint_name        = 'CST NAME'                       
    
    order by cst.constraint_schema, cst.constraint_name           

