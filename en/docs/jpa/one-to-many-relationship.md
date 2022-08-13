---
title: "One to Many relationship"
slug: "one-to-many-relationship"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Parameters
| Annotation| Purpose |
| ------ | ------ |
| @TableGenerator | Specifies generator name and table name where generator can be found|
| @GeneratedValue | Specifies generation strategy and refers to name of generator|
| @ManyToOne| Specifies many to one relationship between Employee and Department|
|@OneToMany(mappedBy="department")| creates bi-directional relationship between Employee and Department by simply referring to @ManyToOne annotation in Employee entity|

## One To Many relationship
One to Many mapping is generally simply a bidirectional relationship of Many to One mapping. We will take same example that we took for Many to one mapping.

**Employee.java**

    @Entity
    public class Employee {
    
        @TableGenerator(name = "employee_gen", table = "id_gen", pkColumnName = "gen_name", valueColumnName = "gen_val", allocationSize = 100)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "employee_gen")
        private int idemployee;
        private String firstname;
        private String lastname;
        private String email;
    
        @ManyToOne
        @JoinColumn(name = "iddepartment")
        private Department department;
    
        // getters and setters    
    }

**Department.java**

    @Entity
    public class Department {
    
        @TableGenerator(table = "id_gen", pkColumnName = "gen_name", valueColumnName = "gen_val", name = "department_gen", allocationSize = 1)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "department_gen")
        private int iddepartment;
        private String name;
    
        @OneToMany(mappedBy = "department")
        private List<Employee> employees;
    
        // getters and setters    
    }

This relationship is represented in database as below. 
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/W13CZ.png

There are two points to remember about jpa one to many mapping:

 - The many to one side is the owning side of relationship. The column
   is defined on that side.
 - The one to many mapping is the inverse side side so the mappedBy
   element must be used on the inverse side.

Complete example can be referred [here][1]


  [1]: http://www.thejavageek.com/2014/01/18/jpa-one-many-mapping/

