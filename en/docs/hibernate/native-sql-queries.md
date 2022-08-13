---
title: "Native SQL Queries"
slug: "native-sql-queries"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Simple Query
Assuming you have a handle on the Hibernate `Session` object, in this case named `session`:

    List<Object[]> result = session.createNativeQuery("SELECT * FROM some_table").list();
    for (Object[] row : result) {
        for (Object col : row) {
            System.out.print(col);
        }
    }

This will retrieve all rows in `some_table` and place them into the `result` variable and print every value.

## Example to get a unique result
    Object pollAnswered = getCurrentSession().createSQLQuery(
            "select * from TJ_ANSWERED_ASW where pol_id = "+pollId+" and prf_log = '"+logid+"'").uniqueResult();

with this query, you get a unique result when you know the result of the query is always going to be unique.

And if the query returns more than one value, you will get an exception 

> org.hibernate.NonUniqueResultException

You also check the details in this link [here with more discription][1]

So, please be sure that you know the query will return unique result


  [1]: https://stackoverflow.com/a/40233705/4374472

