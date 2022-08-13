---
title: "JDBC - Statement Injection"
slug: "jdbc---statement-injection"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

SQL injection is a code injection technique, used to attack data-driven applications, in which nefarious SQL statements are inserted into an entry field for execution (e.g. to dump the database contents to the attacker).

In this section we will talk about that and its relation with JDBC Statement.

## Statement & SQL Injection evil
**Note** in this example we will use PostgreSQL DBMS, but you can use any DBMS

We will use a database `bd_test` witch contain a `Schema: sch_test` and two tables `users` and `test` :

    CREATE TABLE sch_test.users
    (
      id serial NOT NULL,
      username character varying,
      password character varying,
      CONSTRAINT utilisateur_pkey PRIMARY KEY (id)
    ) 

    CREATE TABLE sch_test.test
    (
      id serial NOT NULL,
      "column" character varying
    )

# Simple login using Statement

    static String DRIVER = "org.postgresql.Driver";
    static String DB_USERNAME = "postgres";
    static String DB_PASSWOR = "admin";
    static String DB_URL = "jdbc:postgresql://localhost:5432/bd_test";
    
    public static void sqlInjection() {
        try {
            Class.forName(DRIVER);
            Connection connection = DriverManager.getConnection(DB_URL, DB_USERNAME, DB_PASSWOR);
            Statement statement = connection.createStatement();
            String username = "admin";
            String password = "admin";
            String query = "SELECT * FROM sch_test.users where username = '" 
                    + username + "' and password = '" + password + "'";
    
            ResultSet result = statement.executeQuery(query);
    
            if (result.next()) {
                System.out.println("id = " + result.getInt("id") + " | username = "
                        + result.getString("username") + " | password = " + result.getString("password"));
            }else{
               System.out.println("Login not correct");
            }
    
        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }

Until now every thing is normal and secure.

# Login with fake username and password

The hacker or the tester can simply login or list all your users using this :

    String username = " ' or ''='";
    String password = " ' or ''='";

# INSERT a new user 
You can insert data in your table using :

    String username = "'; INSERT INTO sch_test.utilisateur(id, username, password) 
                            VALUES (2, 'hack1', 'hack2');--";
    String password = "any";

# DELETE All users

consider the hacker know the schema of your database so he can delete all your user 

    String username = "'; DELETE FROM sch_test.utilisateur WHERE id>0;--";
    String password = "any"; 

# DROP Table users
The hacker can also delete your table 

    String username = "'; drop table sch_test.table2;--";
    String password = "any";

# DROP DATABASE

The worst is to drop the database

    String username = "'; DROP DATABASE bd_test;--";
    String password = "any";

and there are many others.

# Why all this?

All this because `Statement` is not secure enough it execute the query like is it, for that it is recommend to use `PreparedStatement` instead, it is more secure that `Statement`.

You can find here more details https://www.wikiod.com/jdbc/preparedstatement


