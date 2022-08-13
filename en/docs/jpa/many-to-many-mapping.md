---
title: "Many to Many Mapping"
slug: "many-to-many-mapping"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

A `ManyToMany` mapping describes a relationship between to entities where both can be related to more than one instance of each other, and is defined by the `@ManyToMany` annotation.

Unlike `@OneToMany` where a foreign key column in the table of the entity can be used, `ManyToMany` requires a join table, which maps the entities to each other. 

## Parameters
| Annotation| Purpose|
| ------ | ------ |
|`@TableGenerator`|Defines a primary key generator that may be referenced by name when a generator element is specified for the `GeneratedValue` annotation |
|`@GeneratedValue`|Provides for the specification of generation strategies for the values of primary keys. It may be applied to a primary key property or field of an entity or mapped superclass in conjunction with the Id annotation.|
|`@ManyToMany`| Specifies relationship between Employee and Project entities such that many employees can work on multiple projects.|
|`mappedBy="projects"`| Defines a bidirectional relationship between Employee and Project|
| `@JoinColumn`| Specifies the name of column that will refer to the Entity to be considered as owner of the association|
|`@JoinTable`| Specifies the table in database which will hold employee to project relationships using foreign keys 


 - @TableGenerator and @GeneratedValue are used for automatic ID
   creation using jpa table generator.
 - @ManyToMany annotation specifies the relationship between Employee
   and Project entities.
 - @JoinTable specifies the name of the table to use as join table jpa
   many to many mapping between Employee and Project using name =
   “employee_project”. This is done because there is no way to determine
   the ownership of a jpa many to many mapping as the database tables do
   not contain foreign keys to reference to other table.
 - @JoinColumn specifies the name of column that will refer to the
   Entity to be considered as owner of the association while
   @inverseJoinColumn specifies the name of inverse side of
   relationship. (You can choose any side to be considered as owner.
   Just make sure those sides in relationship). In our case we have
   chosen Employee as the owner so @JoinColumn refers to idemployee
   column in join table employee_project and @InverseJoinColumn refers
   to idproject which is inverse side of jpa many to many mapping.
 - @ManyToMany annotation in Project entity shows inverse relationship
   hence it uses mappedBy=projects to refer to the field in Employee
   entity.

Full example can be referred [here][1]


  [1]: http://www.thejavageek.com/2014/01/20/jpa-many-many-mapping/

## Employee to Project Many to Many mapping
Employee entity. 

    package com.thejavageek.jpa.entities;
    
    import java.util.List;
    
    import javax.persistence.CascadeType;
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.JoinColumn;
    import javax.persistence.JoinTable;
    import javax.persistence.ManyToMany;
    import javax.persistence.TableGenerator;
    
    @Entity
    public class Employee {
    
        @TableGenerator(name = "employee_gen", table = "id_gen", pkColumnName = "gen_name", valueColumnName = "gen_val", allocationSize = 100)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "employee_gen")
        private int idemployee;
        private String name;
    
        @ManyToMany(cascade = CascadeType.PERSIST)
        @JoinTable(name = "employee_project", joinColumns = @JoinColumn(name = "idemployee"), inverseJoinColumns = @JoinColumn(name = "idproject"))
        private List<Project> projects;
    
        public int getIdemployee() {
            return idemployee;
        }
    
        public void setIdemployee(int idemployee) {
            this.idemployee = idemployee;
        }
    
        public String getName() {
            return name;
        }
    
        public void setName(String name) {
            this.name = name;
        }
    
        public List<Project> getProjects() {
            return projects;
        }
    
        public void setProjects(List<Project> projects) {
            this.projects = projects;
        }
    
    }

Project Entity: 

    package com.thejavageek.jpa.entities;
    
    import java.util.List;
    
    import javax.persistence.CascadeType;
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.ManyToMany;
    import javax.persistence.TableGenerator;
    
    @Entity
    public class Project {
    
        @TableGenerator(name = "project_gen", allocationSize = 1, pkColumnName = "gen_name", valueColumnName = "gen_val", table = "id_gen")
        @Id
        @GeneratedValue(generator = "project_gen", strategy = GenerationType.TABLE)
        private int idproject;
        private String name;
    
        @ManyToMany(mappedBy = "projects", cascade = CascadeType.PERSIST)
        private List<Employee> employees;
    
        public int getIdproject() {
            return idproject;
        }
    
        public void setIdproject(int idproject) {
            this.idproject = idproject;
        }
    
        public String getName() {
            return name;
        }
    
        public void setName(String name) {
            this.name = name;
        }
    
        public List<Employee> getEmployees() {
            return employees;
        }
    
        public void setEmployees(List<Employee> employees) {
            this.employees = employees;
        }
    
    }

Test Code

/* Create EntityManagerFactory */
        EntityManagerFactory emf = Persistence
                .createEntityManagerFactory("JPAExamples");

        /* Create EntityManager */
        EntityManager em = emf.createEntityManager();

        EntityTransaction transaction = em.getTransaction();

        transaction.begin();

        Employee prasad = new Employee();
        prasad.setName("prasad kharkar");

        Employee harish = new Employee();
        harish.setName("Harish taware");

        Project ceg = new Project();
        ceg.setName("CEG");

        Project gtt = new Project();
        gtt.setName("GTT");

        List<Project> projects = new ArrayList<Project>();
        projects.add(ceg);
        projects.add(gtt);

        List<Employee> employees = new ArrayList<Employee>();
        employees.add(prasad);
        employees.add(harish);

        ceg.setEmployees(employees);
        gtt.setEmployees(employees);

        prasad.setProjects(projects);
        harish.setProjects(projects);

        em.persist(prasad);

        transaction.commit();

## How to handle compound key without Embeddable annotation
If You have 

    Role:
    +-----------------------------+
    | roleId | name | discription |
    +-----------------------------+


    Rights:
    +-----------------------------+
    | rightId | name | discription|
    +-----------------------------+

    rightrole
    +------------------+
    | roleId | rightId | 
    +------------------+

In above scenario `rightrole` table has compound key and to access it in JPA user have to create entity with `Embeddable` annotation.

Like this:

**Entity for rightrole table is:**

        @Entity
        @Table(name = "rightrole")
        public class RightRole extends BaseEntity<RightRolePK> {
        
            private static final long serialVersionUID = 1L;
        
            @EmbeddedId
            protected RightRolePK rightRolePK;
    
        
            @JoinColumn(name = "roleID", referencedColumnName = "roleID", insertable = false, updatable = false)
            @ManyToOne(fetch = FetchType.LAZY)
            private Role role;
        
            @JoinColumn(name = "rightID", referencedColumnName = "rightID", insertable = false, updatable = false)
            @ManyToOne(fetch = FetchType.LAZY)
            private Right right;
    
            ......
         }
    
    
        @Embeddable
        public class RightRolePK implements Serializable {
        private static final long serialVersionUID = 1L;
    
          @Basic(optional = false)
          @NotNull
          @Column(nullable = false)
          private long roleID;
    
          @Basic(optional = false)
          @NotNull
          @Column(nullable = false)
         private long rightID;
    
       .....
    
    }

Embeddable annotation is fine for single object but it will give an issue while inserting bulk records.

Problem is whenever user want to create new `role` with `rights` then first user have to `store(persist)` `role` object and then user have to do `flush` to get newly generated `id` for role. then and then user can put it in `rightrole` entity's object.

To solve this user can write entity slightly different way.

**Entity for role table is:**

    @Entity
    @Table(name = "role")
    public class Role {

        private static final long serialVersionUID = 1L;
    
        @Id
        @GeneratedValue(strategy = GenerationType.IDENTITY)
        @Basic(optional = false)
        @NotNull
        @Column(nullable = false)
        private Long roleID;
    
        
        @OneToMany(cascade = CascadeType.ALL, mappedBy = "role", fetch = FetchType.LAZY)
        private List<RightRole> rightRoleList;
     
        @ManyToMany(cascade = {CascadeType.PERSIST})
        @JoinTable(name = "rightrole",
                joinColumns = {
                    @JoinColumn(name = "roleID", referencedColumnName = "ROLE_ID")},
                inverseJoinColumns = {
                    @JoinColumn(name = "rightID", referencedColumnName = "RIGHT_ID")})
        private List<Right> rightList;
    .......
    }


The @JoinTable annotation will take care of inserting in the `rightrole` table even without an entity (as long as that table have only the id columns of role and right). 

User can then simply:

    Role role = new  Role();
    List<Right> rightList = new ArrayList<>();
    Right right1 = new Right();
    Right right2 = new Right();
    rightList.add(right1);
    rightList.add(right2);
    role.setRightList(rightList);

**You have to write @ManyToMany(cascade = {CascadeType.PERSIST}) in inverseJoinColumns otherwise your parent data will get deleted if child get deleted.**

