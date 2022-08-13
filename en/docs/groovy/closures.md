---
title: "Closures"
slug: "closures"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Closure with implicit parameters
```
['cat', 'dog', 'fish'].collect { it.length() }
```

`it` is the default name of the parameter if you have a single parameter and do not explicitly name the parameter. You can optionally declare the parameter as well.

```
['cat', 'dog', 'fish'].collect { animal -> animal.length() }
```

## Closure with explicit parameters
```
def addNumbers = { a, b -> a + b }
addNumbers(-7, 15) // returns 8
```

## Closure with custom target for method calls with implicit receiver
```
class MyHello {
  def sayHello() {
    "Hello, world"
  }
}

def cl = { sayHello() }
cl() // groovy.lang.MissingMethodException    
cl.delegate = new MyHello()
cl(); // "Hello, world"
```

Used extensively by Groovy DSLs.

## Wrapping behavior around a closure with a method
There are frequent behavior patterns that can result in a lot of boilerplate code. By declaring a method that takes a `Closure` as a parameter, you can simplify your program.  As an example, it is a common pattern to retrieve a database connection, start a transaction, do work, and then either commit the transaction, or rollback the connection (in case of error), then finally close the connection:

    def withConnection( String url, String user, String pass, Closure closure) {
        Connection conn = null
        try {
            conn = DriverManager.getConnection( url, user, pass )
            closure.call( conn )
            conn.commit()
        } catch (Exception e) {
            log.error( "DB Action failed", e)
            conn.rollback()
        } finally {
            conn?.close()
        }
    }


    withConnection( DB_PATH, DB_USER, DB_PASS ) { Connection conn ->
        def statement = conn.createStatement()
        def results = statement.executeQuery( 'SELECT * FROM users' )
        // ... more processing ...
    }

## Create closures, assign to properties and call
Let's create a map and a closure to print hello

    def exMap = [:]

    def exClosure = {
        println "Hello"
    }

Assign closure to a property in map
    
    exMap.closureProp = exClosure

Calling closure 
    
    exMap.closureProp.call()

Output
    
    Hello

Another Example - Lets create a class with basic property and assign same closure to object of it

    class Employee {
        def prop
    }
    
    def employee = new Employee()

    employee.prop = exClosure

Call closure through that property

    employee.prop.call()

Output
    
    Hello

## Converting Methods to Closures
A method can be converted to a closure using the **&** operator.
    
    def add(def a, def b) { a + b }    
    
    Closure addClosure = this.&add
    assert this.add(4, 5) == addClosure(4, 5)
    
    





