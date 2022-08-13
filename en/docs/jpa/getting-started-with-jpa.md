---
title: "Getting started with jpa"
slug: "getting-started-with-jpa"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Minimal persistence.xml example

## Hibernate (and embedded H2 DB)

    <?xml version="1.0" encoding="UTF-8"?>
    <persistence xmlns="http://java.sun.com/xml/ns/persistence"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://java.sun.com/xml/ns/persistence
                                 http://java.sun.com/xml/ns/persistence/persistence_2_1.xsd"
             version="2.1">

    <persistence-unit name="persistenceUnit">
        <provider>org.hibernate.ejb.HibernatePersistence</provider>

        <class>my.application.entities.MyEntity</class>
        
        <properties>
            <property name="javax.persistence.jdbc.driver" value="org.h2.Driver" /> 
            <property name="javax.persistence.jdbc.url" value="jdbc:h2:data/myDB.db" /> 
            <property name="javax.persistence.jdbc.user" value="sa" /> 

            <!-- DDL change options -->
            <property name="javax.persistence.schema-generation.database.action" value="drop-and-create"/>
 
            <property name="hibernate.dialect" value="org.hibernate.dialect.H2Dialect"/>
            <property name="hibernate.flushMode" value="FLUSH_AUTO" /> 
        </properties>
    </persistence-unit>
    </persistence>

## Eclipselink (and embedded H2 DB)

    <?xml version="1.0" encoding="UTF-8"?>
    <persistence xmlns="http://java.sun.com/xml/ns/persistence"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://java.sun.com/xml/ns/persistence
                                 http://java.sun.com/xml/ns/persistence/persistence_2_1.xsd"
             version="2.1">

    <persistence-unit name="persistenceUnit">
        <provider>org.eclipse.persistence.jpa.PersistenceProvider</provider>

        <class>my.application.entities.MyEntity</class>
        
        <properties>
            <property name="javax.persistence.jdbc.driver" value="org.h2.Driver"/>
            <property name="javax.persistence.jdbc.url" value="jdbc:h2:data/myDB.db"/>
            <property name="javax.persistence.jdbc.user" value="sa"/>

            <!-- Schema generation : drop and create tables -->
            <property name="javax.persistence.schema-generation.database.action" value="drop-and-create-tables" />
        </properties>
    </persistence-unit>
    
    </persistence>


## DataNucleus (and embedded H2 DB)

    <?xml version="1.0" encoding="UTF-8"?>
    <persistence xmlns="http://java.sun.com/xml/ns/persistence"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://java.sun.com/xml/ns/persistence
                                 http://java.sun.com/xml/ns/persistence/persistence_2_1.xsd"
             version="2.1">

    <persistence-unit name="persistenceUnit">
        <provider>org.datanucleus.api.jpa.PersistenceProviderImpl</provider>

        <class>my.application.entities.MyEntity</class>
        
        <properties>
            <property name="javax.persistence.jdbc.driver" value="org.h2.Driver"/>
            <property name="javax.persistence.jdbc.url" value="jdbc:h2:data/myDB.db"/>
            <property name="javax.persistence.jdbc.user" value="sa"/>

            <!-- Schema generation : drop and create tables -->
            <property name="javax.persistence.schema-generation.database.action" value="drop-and-create-tables" />
        </properties>
    </persistence-unit>
    
    </persistence>



## Hello World
Let's see all the basic component for create a simple Hallo World.

1. Define which implementation of JPA 2.1 we will use
2. Build the connection to database creating the `persistence-unit`
3. Implements the entities
4. Implements DAO (Data access object) for manipulate the entities
5. Test the application

# Libraries
Using maven, we need this dependancies:

    <dependencies>

        <!-- JPA is a spec, I'll use the implementation with HIBERNATE -->
        <dependency>
            <groupId>org.hibernate</groupId>
            <artifactId>hibernate-entitymanager</artifactId>
            <version>5.2.6.Final</version>
        </dependency>

        <!-- JDBC Driver, use in memory DB -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <version>1.4.193</version>
        </dependency>

    </dependencies>
 
# Persistence Unit
In the resources folder we need to create a file called `persistence.xml`.
The easiest way for define it is like this:
<?xml version="1.0" encoding="UTF-8"?>
<persistence>

    <persistence-unit name="hello-jpa-pu" transaction-type="RESOURCE_LOCAL">
        <provider>org.hibernate.jpa.HibernatePersistenceProvider</provider>

        <properties>
            <!-- ~ = relative to current user home directory -->
            <property name="javax.persistence.jdbc.url" value="jdbc:h2:./test.db"/>
            <property name="javax.persistence.jdbc.user" value=""/>
            <property name="javax.persistence.jdbc.password" value=""/>
            <property name="javax.persistence.jdbc.driver" value="org.h2.Driver"/>
            <property name="hibernate.dialect" value="org.hibernate.dialect.H2Dialect"/>
            <property name="hibernate.show_sql" value="true"/>

            <!-- This create automatically the DDL of the database's table -->
            <property name="hibernate.hbm2ddl.auto" value="create-drop"/>

        </properties>
    </persistence-unit>
</persistence>

# Implement an Entity
I create a class `Biker`:

    package it.hello.jpa.entities;
    
    
    import javax.persistence.*;
    import java.io.Serializable;
    import java.util.Date;
    import java.util.List;
    
    @Entity
    @Table(name = "BIKER")
    public class Biker implements Serializable {
    
        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        private Long id;
    
        @Column(name = "bikerName")
        private String name;
    
        @Column(unique = true, updatable = false)
        private String battleName;
    
        private Boolean beard;
    
        @Temporal(TemporalType.DATE)
        private Date birthday;
    
        @Temporal(TemporalType.TIME)
        private Date registrationDate;
    
        @Transient // --> this annotiation make the field transient only for JPA
        private String criminalRecord; 
    
        public Long getId() {
            return this.id;
        }
    
        public void setId(Long id) {
            this.id = id;
        }
    
        public String getName() {
            return this.name;
        }
    
        public void setName(String name) {
            this.name = name;
        }
    
        public String getBattleName() {
            return battleName;
        }
    
        public void setBattleName(String battleName) {
            this.battleName = battleName;
        }
    
        public Boolean getBeard() {
            return this.beard;
        }
    
        public void setBeard(Boolean beard) {
            this.beard = beard;
        }
    
        public Date getBirthday() {
            return birthday;
        }
    
        public void setBirthday(Date birthday) {
            this.birthday = birthday;
        }
    
        public Date getRegistrationDate() {
            return registrationDate;
        }
    
        public void setRegistrationDate(Date registrationDate) {
            this.registrationDate = registrationDate;
        }
    
        public String getCriminalRecord() {
            return criminalRecord;
        }
    
        public void setCriminalRecord(String criminalRecord) {
            this.criminalRecord = criminalRecord;
        }
    }

# Implement a DAO

    package it.hello.jpa.business;
    
    import it.hello.jpa.entities.Biker;
    
    import javax.persistence.EntityManager;
    import java.util.List;
    
    public class MotorcycleRally {
    
        public Biker saveBiker(Biker biker) {
            EntityManager em = EntityManagerUtil.getEntityManager();
            em.getTransaction().begin();
            em.persist(biker);
            em.getTransaction().commit();
            return biker;
        }
   
    }

`EntityManagerUtil` is a singleton:

    package it.hello.jpa.utils;
    
    import javax.persistence.EntityManager;
    import javax.persistence.EntityManagerFactory;
    import javax.persistence.Persistence;
    
    public class EntityManagerUtil {
    
        // USE THE SAME NAME IN persistence.xml!
        public static final String PERSISTENCE_UNIT_NAME = "hello-jpa-pu";
    
        private static EntityManager entityManager;
    
        private EntityManagerUtil() {
        }
    
        public static EntityManager getEntityManager() {
            if (entityManager == null) {
                // the same in persistence.xml
                EntityManagerFactory emFactory = Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME);
    
    
                return emFactory.createEntityManager();
            }
            return entityManager;
        }
    }

# Test the application
package it.hello.jpa.test;

public class TestJpa {

    @Test
    public void insertBiker() {
        MotorcycleRally crud = new MotorcycleRally();

        Biker biker = new Biker();
        biker.setName("Manuel");
        biker.setBeard(false);

        biker = crud.saveBiker(biker);

        Assert.assertEquals(biker.getId(), Long.valueOf(1L));
    }

}

The output will be:

> Running it.hello.jpa.test.TestJpa Hibernate: drop table BIKER if
> exists Hibernate: drop sequence if exists hibernate_sequence
> Hibernate: create sequence hibernate_sequence start with 1 increment
> by 1 Hibernate: create table BIKER (id bigint not null, battleName
> varchar(255), beard boolean, birthday date, bikerName varchar(255),
> registrationDate time, primary key (id)) Hibernate: alter table BIKER
> add constraint UK_a64ce28nywyk8wqrvfkkuapli unique (battleName)
> Hibernate: insert into BIKER (battleName, beard, birthday, bikerName,
> registrationDate, id) values (?, ?, ?, ?, ?, ?) mar 01, 2017 11:00:02
> PM org.hibernate.jpa.internal.util.LogHelper
> logPersistenceUnitInformation INFO: HHH000204: Processing
> PersistenceUnitInfo [
>     name: hello-jpa-pu
>     ...]    Results :
> 
> Tests run: 1, Failures: 0, Errors: 0, Skipped: 0



## Installation or Setup
## Classpath requirements

### Eclipselink
The Eclipselink and JPA API need to be included. Example Maven dependencies:

    <dependencies>
      <dependency>
        <groupId>org.eclipse.persistence</groupId>
        <artifactId>eclipselink</artifactId>
        <version>2.6.3</version>
      </dependency>
      <dependency>
        <groupId>org.eclipse.persistence</groupId>
        <artifactId>javax.persistence</artifactId>
        <version>2.1.1</version>
      </dependency>
      <!-- ... -->
    </dependencies>

### Hibernate 
Hibernate-core is required. Example Maven dependency:

    <dependencies>
      <dependency>
        <!-- requires Java8! -->
        <!-- as of 5.2, hibernate-entitymanager is merged into hibernate-core -->
        <groupId>org.hibernate</groupId>
        <artifactId>hibernate-core</artifactId>
        <version>5.2.1.Final</version>
      </dependency>
      <dependency>
        <groupId>org.hibernate.javax.persistence</groupId>
        <artifactId>hibernate-jpa-2.1-api</artifactId>
        <version>1.0.0</version>
      </dependency>
      <!-- ... -->
    </dependencies>



### DataNucleus 
datanucleus-core, datanucleus-api-jpa and datanucleus-rdbms (when using RDBMS datastores) are required. Example Maven dependency:

    <dependencies>
      <dependency>
        <groupId>org.datanucleus</groupId>
        <artifactId>datanucleus-core</artifactId>
        <version>5.0.0-release</version>
      </dependency>
      <dependency>
        <groupId>org.datanucleus</groupId>
        <artifactId>datanucleus-api-jpa</artifactId>
        <version>5.0.0-release</version>
      </dependency>
      <dependency>
        <groupId>org.datanucleus</groupId>
        <artifactId>datanucleus-rdbms</artifactId>
        <version>5.0.0-release</version>
      </dependency>
      <dependency>
        <groupId>org.datanucleus</groupId>
        <artifactId>javax.persistence</artifactId>
        <version>2.1.2</version>
      </dependency>
      <!-- ... -->
    </dependencies>


## Configuration Details

JPA requires the use of a file *persistence.xml*, located under `META-INF` from the root of the CLASSPATH. This file contains a definition of the available persistence units from which JPA can operate. 

JPA additionally allows use of a mapping configuration file *orm.xml*, also placed under `META-INF`. This mapping file is used to configure how classes are mapped to the datastore, and is an alternative/supplement to use of Java annotations in the JPA entity classes themselves.

