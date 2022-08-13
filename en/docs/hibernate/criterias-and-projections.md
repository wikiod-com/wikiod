---
title: "Criterias and Projections"
slug: "criterias-and-projections"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Use Filters
`@Filter` is used as a `WHERE` camp, here some examples

Student Entity

    @Entity
    @Table(name = "Student")
    public class Student
    {
        /*...*/
    
        @OneToMany
        @Filter(name = "active", condition = "EXISTS(SELECT * FROM Study s WHERE state = true and s.id = study_id)")
        Set<StudentStudy> studies;
    
        /* getters and setters methods */
    }


Study Entity

    @Entity
    @Table(name = "Study")
    @FilterDef(name = "active")
    @Filter(name = "active", condition="state = true")
    public class Study
    {
        /*...*/
    
        @OneToMany
        Set<StudentStudy> students;
    
        @Field
        boolean state;
    
        /* getters and setters methods */
    }

StudentStudy Entity

    @Entity
    @Table(name = "StudentStudy")
    @Filter(name = "active", condition = "EXISTS(SELECT * FROM Study s WHERE state = true and s.id = study_id)")
    public class StudentStudy
    {
        /*...*/
    
        @ManytoOne
        Student student;
    
        @ManytoOne
        Study study;
    
        /* getters and setters methods */
    }



This way, everytime the "active" filter is enabled, 

-Every query we do on the student entity will return **ALL** Students with **ONLY** their `state = true` studies

-Every query we do on the Study entity will return **ALL** `state = true` studies

-Every query we do on the StudentStudy entiy will return **ONLY** the ones with a `state = true` Study relationship


Pls note that study_id is the name of the field on the sql StudentStudy table 

## List using Restrictions
Assuming we have a TravelReview table with City names as column "title"

     Criteria criteria =
        session.createCriteria(TravelReview.class);
      List review =
        criteria.add(Restrictions.eq("title", "Mumbai")).list();
      System.out.println("Using equals: " + review);

We can add restrictions to the criteria by chaining them as follows:

    List reviews = session.createCriteria(TravelReview.class)
       .add(Restrictions.eq("author", "John Jones"))
       .add(Restrictions.between("date",fromDate,toDate))
       .add(Restrictions.ne("title","New York")).list();




## Using Projections
Should we wish to retrieve only a few columns, we can use the  Projections class to do
so. For example, the following code retrieves the  title column

     // Selecting all title columns
      List review = session.createCriteria(TravelReview.class)
            .setProjection(Projections.property("title"))
        .list();
      // Getting row count
      review = session.createCriteria(TravelReview.class)
        .setProjection(Projections.rowCount())
        .list();
      // Fetching number of titles
      review = session.createCriteria(TravelReview.class)
        .setProjection(Projections.count("title"))
        .list();

