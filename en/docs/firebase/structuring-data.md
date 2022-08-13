---
title: "Structuring Data"
slug: "structuring-data"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Firebase database is a NoSQL database that stores its data in the form of hierarchal JSON objects. There are no tables or records of any form as an SQL database would normally have, just nodes that make up a key-value structure.

**Data Normalization**

In order to have a properly designed database structure, the data requirements must be thoroughly outlined and forethought. The structure in this case should be normalized; the more flat the JSON tree, the faster data-access is.

## Do's and Don'ts
**The Wrong Way**

Consider the following structure

    {
      "users": {

        // Uniquely generated IDs for children is common practice,
        // it's actually really useful for automating child creation.
        // Auto-incrementing an integer for a key can be problematic when a child is removed.

        "-KH3Cx0KFvSQELIYZezv": {     
          "name": "Jon Snow",
          "aboutMe": "I know nothing...",
          "posts": {
            "post1": { 
              "body": "Different roads sometimes leads to the same castle", 
              "isHidden": false
            },
            "post2": { ... },
            // Possibly more posts
          }
        },
        "-KH3Dx2KFdSLErIYZcgk": { ... },     // Another user
        // A lot more users here
      }
    }

This is a great example of what ***NOT*** to do. Multi-nested structures such as the one above can be very problematic and could cause a huge performance setback. 

The way Firebase accesses a node is by downloading all the children's data, then iterating over all same-level nodes (all parents' children). Now, imagine a database with several  *users*, each having hundreds (or even thousands) of *posts*. Accessing a *post* in this case could potentially load hundreds of megabytes of unused data. In a more complicated application, the nesting could be deeper than just 4 layers, which would result in more useless downloads and iterations.

**The Right Way**

Flattening the same structure would look like this

    {
      // "users" should not contain any of the posts' data
      "users": {
        "-KH3Cx0KFvSQELIYZezv": {
          "name": "Jon Snow",
          "aboutMe": "I know nothing..."
        },
        "-KH3Dx2KFdSLErIYZcgk": { ... },
        // More users
      },

      // Posts can be accessed provided a user key
      "posts": {
        "-KH3Cx0KFvSQELIYZezv": {     // Jon Snow's posts
          "post1": { 
            "body": "Different roads sometimes leads to the same castle", 
            "isHidden": false
          },
          "post2": { ... },
          // Possibly more posts
        },
        "-KH3Dx2KFdSLErIYZcgk": { ... },
        // other users' posts
      }
    }

This spares a huge amount of overhead by iterating over less nodes to access a target object. All *users* that do not have any posts would not exist in the *posts* branch, and so iterating over those users in *the wrong way* above is completely useless.

## Two-Way Relationships
The following is an example of a simple and minimal college database that uses two-way relationships

    {
      "students": {
        "-SL3Cs0KFvDMQLIYZEzv": {
          "name": "Godric Gryffindor",
          "id": "900130309",
          "courses": {
             "potions": true,
             "charms": true,
             "transfiguration": true, 
          }
        },
        "-SL3ws2KvZQLTYMqzSas": {          
          "name": "Salazar Slytherin",
          "id": "900132319",
          "courses": {
             "potions": true,
             "herbs": true,
             "muggleStudies": true, 
          }
        },
        "-SL3ns2OtARSTUMywqWt": { ... },
        // More students here
      },

      "courses": {
        "potions": {
          "code": "CHEM305",
          "enrolledStudents": {
            "-SL3Cs0KFvDMQLIYZEzv": true,     // Godric Gryffindor
            "-SL3ws2KvZQLTYMqzSas": true,     // Salazar Slytherin
            // More students
          }
        },
        "muggleStuddies": {
          "code": "SOC215",
          "enrolledStudents": {
            "-SL3ws2KvZQLTYMqzSas": true,     // Salazar Slytherin
            "-SL3ns2OtARSTUMywqWt": true,     // Some other student
            // More students
          }
        },
        // More courses
      }
    }

Note that each student has a list of *courses* and each course has a list of enrolled *students*.

Redundancy is not always a bad approach. It's true that it costs storage space and having to deal with multiple entries' updating when deleting or editing a duplicated node; however, in some scenarios where data is not updated often, having two-way relationships could ease the fetching/writing process significantly.

In most scenarios where an SQL-like query seems needed, inverting the data and creating two-way relationships is usually the solution. 

Consider an application using the database above that requires the ability to:
 1. List the courses a certain student is taking **and**...
 2. List all the students in a certain course 

If the database structure had been one-directional, it would incredibly slower to scan or query for one of the two requirements above. In some scenarios, redundancy makes frequent operations faster and much more efficient which, on the long run, makes the duplications' cost negligible.

