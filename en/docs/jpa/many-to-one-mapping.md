---
title: "Many To One Mapping"
slug: "many-to-one-mapping"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Parameters
| Column | Column |
| ------ | ------ |
| @TableGenerator| Uses table generator strategy for automatic id creation   |
|@GeneratedValue| Specifies that the value applied to fields is a generated value|
|@Id | Annotates the field as identifier|
|@ManyToOne | Specifies Many to One relationship between Employee and Department. This annotation is marked on many side. i.e. Multiple employees belong to a single department. So Department is annotated with @ManyToOne in Employee entity.|
| @JoinColumn| Specifies database table column which stores foreign key for related entity|

## Employee to Department ManyToOne relationship
Employee Entity
    
    @Entity
    public class Employee {
    
        @TableGenerator(name = "employee_gen", table = "id_gen", pkColumnName = "gen_name", valueColumnName = "gen_val", allocationSize = 1)
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
        // toString implementation
    }

Department Entity

    @Entity
    public class Department {
    
        @Id
        private int iddepartment;
        private String name;
    
        // getters, setters and toString()
    }

**Test class**

    public class Test {
    
        public static void main(String[] args) {
    
            EntityManagerFactory emf = Persistence
                    .createEntityManagerFactory("JPAExamples");
            EntityManager em = emf.createEntityManager();
            EntityTransaction txn = em.getTransaction();
    
            Employee employee = new Employee();
            employee.setEmail("someMail@gmail.com");
            employee.setFirstname("Prasad");
            employee.setLastname("kharkar");
    
            txn.begin();
            Department department = em.find(Department.class, 1);//returns the department named vert
            System.out.println(department);
            txn.commit();
    
            employee.setDepartment(department);
    
            txn.begin();
            em.persist(employee);
            txn.commit();
    
        }
    
    }

