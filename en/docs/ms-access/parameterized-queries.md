---
title: "Parameterized Queries"
slug: "parameterized-queries"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Parameterized Queries can be used to defend against SQL Injection attacks.

## Vulnerable Approach: Concatenated SQL string with form references
This is the typical approach for novice developers building SQL action queries. They are vulnerable to the [Bobby Tables](https://xkcd.com/327/) type SQL Injection attacks.

    Dim strSQL As String

    strSQL = "INSERT INTO Employees chrFirstName, chrLastName, chrPhone " _
             & "VALUES ('" & Me!txtFirstName & "','" & Me!txtLastName & "','" & Me!txtPhone & "');"

    CurrentDb.Execute strSQL

## QueryDef Parameterized Query Approach
This approach will prevent a user from embedding a second SQL statement in their input for execution.

    Dim strSQL As String
    Dim db As DAO.Database
    Dim qdf As DAO.QueryDef

    strSQL = "PARAMETERS [FirstName] Text(255), [LastName] Text(255), [Phone] Text(255); " _
             & "INSERT INTO Employees (chrFirstName, chrLastName, chrPhone) " _
             & "VALUES ([FirstName], [LastName], [Phone]);"

    Set db = CurrentDb

    Set qdf = db.CreateQueryDef("", strSQL)
    qdf.Parameters("FirstName") = Me!txtFirstName
    qdf.Parameters("LastName") = Me!txtLastName
    qdf.Parameters("Phone") = Me!txtPhone
    qdf.Execute

    Me!txtFirstName = vbNullString
    Me!txtLastName = vbNullString
    Me!txtPhone = vbNullString
    
    qdf.Close
    db.Close
    Set qdf = Nothing
    Set db = Nothing
    
Valid Parameter Types:

 - `DATETIME`: for dates (parameter expects VBA `Date`)
 - `SHORT`,`LONG`: For integers (`SHORT` expects Integer, `LONG` expects Long)
 - `SINGLE`,`DOUBLE` : For floating point (expect Single and Double respectively)
 - `VARCHAR` or `TEXT`: For strings
 - `MEMO` or `LONGTEXT`: For strings longer than 255 characters

