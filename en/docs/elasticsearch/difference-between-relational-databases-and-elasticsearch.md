---
title: "Difference Between Relational Databases and Elasticsearch"
slug: "difference-between-relational-databases-and-elasticsearch"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

This is for the readers who come from relational background and want to learn elasticsearch. This topic shows the use cases for which Relational databases are not a suitable option.

## Terminology Difference
| **Relational Database** | **Elasticsearch** |
| ------ | ------ |
| Database   | Index   |
| Table   | Type   |
| Row/Record   | Document   |
| Column Name   | field   |

Above table roughly draws an analogy between basic elements of relational database and elasticsearch.

**Setup**

 Considering Following structure in a relational database:

   
    create databse test;
    
    use test;
    
    create table product;
    
    create table product (name varchar, id int PRIMARY KEY);
    
    insert into product  (id,name) VALUES (1,'Shirt');
    
    insert into product  (id,name) VALUES (2,'Red Shirt');
    
    select * from product;
    
    name      | id 
    ----------+----
    Shirt     |  1
    Red Shirt |  2


**Elasticsearch Equivalent:**


    POST test/product
    {
        "id" : 1,
        "name" : "Shirt"
    }

    POST test/product
    {
        "id" : 2,
        "name" : "Red Shirt"
    }

    GET test/product/_search

  
    "hits": [
         {                                      ==============
            "_index": "test",                   ===> index    |
            "_type": "product",                 ===>type      |
            "_id": "AVzglFomaus3G2tXc6sB",                    |
            "_score": 1,                                      |
            "_source": {                                      |===> document
               "id": 2,                        ===>field      | 
               "name": "Red Shirt"             ===>field      |
            }                                                 |
         },                                     ==============
         {
            "_index": "test",
            "_type": "product",
            "_id": "AVzglD12aus3G2tXc6sA",
            "_score": 1,
            "_source": {
               "id": 1,                 
               "name": "Shirt"           
            }
         }
      ]
          



## Usecases where Relational Databases are not suitable
 - Essence of searching lies in its order. Everyone wants search results to be shown in such a way that best suited results are shown on top. Relational database do not have such capability. Elasticsearch on the other hand shows results on the basis of relevancy by default.

   **Setup**

   Same as used in previous example.

   **Problem Statement**
  
   *Suppose user wants to search for `shirts` but he is interested in `red` colored shirts. In that case, results containing `red` and `shirts` keyword should come on top. Then results for other shirts should be shown after them.*

   **Solution Using Relational Database Query**

   `select * from product where name like '%Red%' or name like  '%Shirt%'`;

    **Output**

       name       | id 
       -----------+----
       Shirt      |  1
       Red Shirt  |  2

    **Elasticsearch Solution**

       POST test/product/_search
       {
            "query": {
                 "match": {
                   "name": "Red Shirt"
                }
            }
       }

    **Output**
    
       "hits": [
         {
            "_index": "test",
            "_type": "product",
            "_id": "AVzglFomaus3G2tXc6sB",
            "_score": 1.2422675,              ===> Notice this
            "_source": {
               "id": 2,
               "name": "Red Shirt"
            }
         },
         {
            "_index": "test",
            "_type": "product",
            "_id": "AVzglD12aus3G2tXc6sA",
            "_score": 0.25427115,             ===> Notice this
            "_source": {
               "id": 1,
               "name": "Shirt"
            }
         }
        ]

     **Conclusion**

   As we can see above Relational Database has returned results in some random order, while Elasticsearch returns results in decreasing order of `_score` which is calculated on the basis of relevancy.


----------


 - We tend to make mistakes while entering search string. There are cases when user enters an incorrect search parameter. Relational Databases won't handle such cases. Elasticsearch to the rescue.

   **Setup**

   Same as used in previous example.

   **Problem Statement**
  
   *Suppose user wants to search for `shirts` but he enters an incorrect word `shrt` by mistake. User still expects to see the results of shirt*.

   **Solution Using Relational Database Query**

   `select * from product where name like '%shrt%'`;

    **Output**

       No results found

    **Elasticsearch Solution** 
      
       POST /test/product/_search
 
        {
           "query": {
             "match": {
               "name": {
                 "query": "shrt",
                 "fuzziness": 2,
                 "prefix_length": 0
                }
             }
           }
        }  

   **Output**
    
        "hits": [
         {
            "_index": "test",
            "_type": "product",
            "_id": "AVzglD12aus3G2tXc6sA",
            "_score": 1,
            "_source": {
               "id": 1,
               "name": "Shirt"
            }
         },
         {
            "_index": "test",
            "_type": "product",
            "_id": "AVzglFomaus3G2tXc6sB",
            "_score": 0.8784157,
            "_source": {
               "id": 2,
               "name": "Red Shirt"
            }
         }
       ]

     **Conclusion**

   As we can see above relational database has returned no results for an incorrect word searched, while Elasticsearch using its special `fuzzy` query returns results.



   
    

    


