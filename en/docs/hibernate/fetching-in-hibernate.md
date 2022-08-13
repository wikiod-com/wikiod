---
title: "Fetching in hibernate"
slug: "fetching-in-hibernate"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Fetching is really important in JPA (Java Persistence API). In JPA, HQL(Hibernate Query Language) and JPQL(Java Persistence Query Language) are used to fetch the entities based on their relationships. Although it is way better than using so many joining queries and sub-queries to get what we want by using native SQL, the strategy how we fetch the associated entities in JPA are still essentially effecting the performance of our application.

## It is recommended to use FetchType.LAZY. Join fetch the columns when they are needed.
Below is an Employer entity class which is mapped to the table employer. As you can see I used **fetch = FetchType.LAZY** instead of fetch = FetchType.EAGER. The reason I am using LAZY is because Employer may have a lot of properties later on and every time I may not need to know all the fields of an Employer, so loading all of them will leading a bad performance then an employer is loaded. 

    @Entity
    @Table(name = "employer")
        public class Employer  
        {  
            @Id
            @GeneratedValue(strategy = GenerationType.IDENTITY)
            private Long id;
        
            @Column(name = "name")
            private String Name;
    
            @OneToMany(mappedBy = "employer", fetch = FetchType.LAZY,
                   cascade = { CascadeType.ALL }, orphanRemoval = true)
            private List<Employee> employees;
      
            public Long getId() {  
                return id;  
            }
    
            public void setId(Long id) {  
                this.id = id;  
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

However, for LAZY fetched associations, uninitialized proxies are sometimes leads to LazyInitializationException. In this case, we can simply use JOIN FETCH in the HQL/JPQL to avoid LazyInitializationException. 

    SELECT Employer employer FROM Employer
           LEFT JOIN FETCH employer.name
           LEFT JOIN FETCH employer.employee employee 
           LEFT JOIN FETCH employee.name
           LEFT JOIN FETCH employer.address



