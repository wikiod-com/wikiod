---
title: "Basic mapping"
slug: "basic-mapping"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Parameters
| Annotation | Details |
| ---------- | ------- |
| `@Id` | Marks field/column as the _key_ of the entity | 
| `@Basic` | Marks requested field to mapped as a _basic_ type. This is applicable to primitive types and their wrappers, `String`, `Date` and `Calendar`. The annotation is actually optional if no parameters are given, but good style would dictate to make your intentions explicit. | 
| `@Transient` | Fields marked as transient are not considered for persistence, much like the `transient` keyword for serialization. |



There always needs to be a default constructor, that is, the parameterless one. In the basic example, there was no constructor specified, so Java added one; but if you add a constructor with arguments, be sure to add the parameterless constructor, too.

## A very simple entity
    @Entity
    class Note {
        @Id
        Integer id;
     
        @Basic
        String note;
    
        @Basic
        int count;
    }

Getters, setters etc. are ommitted for brevity, but they are not needed for JPA anyway.

This Java class would map to the following table (depending on your database, here given in one possible Postgres mapping):

    CREATE TABLE Note (
      id integer NOT NULL,
      note text,
      count integer NOT NULL
    )

JPA providers may be used to generate the DDL, and will likely produce DDL different from the one shown here, but as long as the types are compatible, this will not cause problems at runtime. It is best not to rely on auto-generation of DDL.

## Omitting field from the mapping
    @Entity
    class Note {
        @Id
        Integer id;
     
        @Basic
        String note;
    
        @Transient
        String parsedNote;

        String readParsedNote() {
            if (parsedNote == null) { /* initialize from note */ }
            return parsedNote;
        }
    }

If your class needs fields that should not be written to the database, mark them as `@Transient`. After reading from the database, the field will be `null`.

## Mapping time and date
Time and date come in a number of different types in Java: The now historic `Date` and `Calendar`, and the more recent `LocalDate` and `LocalDateTime`. And `Timestamp`, `Instant`, `ZonedLocalDateTime` and the Joda-time types. On the database side, we have `time`, `date` and `timestamp` (both time and date), possibly with or without time zone.

Date and time before Java 8
===========================

The _default_ mapping for the pre-Java-8 types `java.util.Date`, `java.util.Calendar` and `java.sql.Timestamp` is `timestamp` in SQL; for `java.sql.Date` it is `date`.

    @Entity
    class Times {
        @Id
        private Integer id;
    
        @Basic
        private Timestamp timestamp;
    
        @Basic
        private java.sql.Date sqldate;
    
        @Basic
        private java.util.Date utildate;
    
        @Basic
        private Calendar calendar;
    }

This will map perfectly to the following table:

<!-- language: lang-sql -->
    CREATE TABLE times (
        id integer not null,
        timestamp timestamp,
        sqldate date,
        utildate timestamp,
        calendar timestamp
    )

This may not be the intention. For instance, often a Java `Date` or `Calendar` is used to represent the date only (for date of birth). To change the default mapping, or just to make the mapping explicit, you can use the `@Temporal` annotation.

    @Entity
    class Times {
        @Id
        private Integer id;
    
        @Temporal(TemporalType.TIME)
        private Date date;
    
        @Temporal(TemporalType.DATE)
        private Calendar calendar;
    }

The equivalent SQL table is:

<!-- language: lang-sql -->
    CREATE TABLE times (
        id integer not null,
        date time,
        calendar date
    )

_Note 1:_ The type specified with `@Temporal` influences DDL generation; but you can also have a colum of type `date` map to `Date` with just the `@Basic` annotation.

_Note 2:_ `Calendar` cannot persist `time` only.


Date and time with Java 8
=========================
JPA 2.1 does _not_ define support for `java.time` types provided in Java 8. The majority of JPA 2.1 implementations offer support for these types however, though these are strictly speaking vendor extensions.

For DataNucleus, these types just work out of the box, and offers a wide range of mapping possibilities, coupling in with the `@Temporal` annotation.

For Hibernate, if using Hibernate 5.2+ they should work out of the box, just using the `@Basic` annotation. If using Hibernate 5.0-5.1 you need to add the dependency `org.hibernate:hibernate-java8`. The mappings provided are
 * `LocalDate` to `date`
 * `Instant`, `LocalDateTime` and `ZonedDateTime` to `timestamp`

A vendor-neutral alternative would also be to define a JPA 2.1 `AttributeConverter` for any Java 8 `java.time` type that is required to be persisted.

## Entity with sequence managed Id
Here we have a class and we want the identity field (`userUid`) to have its value generated via a SEQUENCE in the database. This SEQUENCE is assumed to be called `USER_UID_SEQ`, and can be created by a DBA, or can be created by the JPA provider.


    @Entity
    @Table(name="USER")
    public class User implements Serializable {
        private static final long serialVersionUID = 1L;

        @Id
        @SequenceGenerator(name="USER_UID_GENERATOR", sequenceName="USER_UID_SEQ")
        @GeneratedValue(strategy=GenerationType.SEQUENCE, generator="USER_UID_GENERATOR")
        private Long userUid;

        @Basic
        private String userName;
    }

