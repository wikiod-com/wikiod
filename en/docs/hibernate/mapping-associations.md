---
title: "Mapping associations"
slug: "mapping-associations"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## One to One Hibernate Mapping
Every Country has one Capital. Every Capital has one Country.

**Country.java**

    package com.entity;
    import javax.persistence.Column;
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.OneToOne;
    import javax.persistence.Table;
    
    @Entity
    @Table(name = "countries")
    public class Country {
        @Id
        @GeneratedValue(strategy = GenerationType.IDENTITY)
        private int id;

        @Column(name = "name")
        private String name;

        @Column(name = "national_language")
        private String nationalLanguage;

        @OneToOne(mappedBy = "country")
        private Capital capital;
    
        //Constructor
    
        //getters and setters
    
       }

**Capital.java**

    package com.entity;

    import javax.persistence.CascadeType;
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.JoinColumn;
    import javax.persistence.OneToOne;
    import javax.persistence.Table;

    @Entity
    @Table(name = "capitals")
    public class Capital {

        @Id
        @GeneratedValue(strategy = GenerationType.IDENTITY)
        private int id;

        private String name;

        private long population;

        @OneToOne(cascade = CascadeType.ALL)
        @JoinColumn(name = "country_id")
        private Country country;

        //Constructor

        //getters and setters

    }


**HibernateDemo.java**

    package com.entity;
    import org.hibernate.Session;
    import org.hibernate.SessionFactory;
    import org.hibernate.cfg.Configuration;

    public class HibernateDemo {

    public static void main(String ar[]) {
        SessionFactory sessionFactory = new Configuration().configure().buildSessionFactory();
        Session session = sessionFactory.openSession();
        Country india = new Country();
        Capital delhi = new Capital();
        delhi.setName("Delhi");
        delhi.setPopulation(357828394);
        india.setName("India");
        india.setNationalLanguage("Hindi");
        delhi.setCountry(india);
        session.save(delhi);
        session.close();
      }

    }

