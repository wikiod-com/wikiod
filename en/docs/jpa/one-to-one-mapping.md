---
title: "One to One mapping"
slug: "one-to-one-mapping"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| Annotation| Purpose |
| ------ | ------ |
| @TableGenerator   | Specifies generator name and table name where generator can be found|
| @GeneratedValue | Specifies generation strategy and refers to name of generator|
| @OneToOne| Specifies one to one relationship between employee and desk, here Employee is owner of relation|
|mappedBy| This element is provided on reverse side of relation. This enables bidirectional relationship|


## One To One relation between employee and desk
Consider a one to one  bidirectional relationship between employee and desk. 

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
    
        @OneToOne
        @JoinColumn(name = "iddesk")
        private Desk desk;
    
        // getters and setters    
    }

**Desk.java**

    @Entity
    public class Desk {
    
        @TableGenerator(table = "id_gen", name = "desk_gen", pkColumnName = "gen_name", valueColumnName = "gen_value", allocationSize = 1)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "desk_gen")
        private int iddesk;
        private int number;
        private String location;
        @OneToOne(mappedBy = "desk")
        private Employee employee;

        // getters and setters    
    }

**Test Code**

    /* Create EntityManagerFactory */
        EntityManagerFactory emf = Persistence
                .createEntityManagerFactory("JPAExamples");

        /* Create EntityManager */
        EntityManager em = emf.createEntityManager();

        Employee employee;

        employee = new Employee();
        employee.setFirstname("pranil");
        employee.setLastname("gilda");
        employee.setEmail("sdfsdf");

        Desk desk = em.find(Desk.class, 1); // retrieves desk from database
        employee.setDesk(desk);

        em.persist(employee);

        desk = em.find(Desk.class, 1); // retrieves desk from database
        desk.setEmployee(employee);
        System.out.println(desk.getEmployee());

Database diagram is depicted as below.
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/cBhfQ.png
 - The **@JoinColumn** annotation goes on mapping of the entity that is
   mapped to the table containing the join colulmn.The owner of
   relationship. In our case, Employee table has the join column so
   **@JoinColumn** is on Desk field of Employee entity.
 - The **mappedBy** element should be specified in the **@OneToOne** association
   in the entity that reverse side of relationship. i.e. The entity
   which does not provide join column on database aspect. In our case,
   Desk is the inverse entity.

Complete example can be found [here][1]


  [1]: http://www.thejavageek.com/2014/01/17/jpa-one-one-mapping/

