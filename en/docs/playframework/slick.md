---
title: "Slick"
slug: "slick"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Slick getting started code
In `build.sbt`, make sure you include (here for Mysql and PostGreSQL):

     "mysql" % "mysql-connector-java" % "5.1.20",  
     "org.postgresql" % "postgresql" % "9.3-1100-jdbc4",
     "com.typesafe.slick" %% "slick" % "3.1.1",
     "com.typesafe.play" %% "play-slick" % "1.1.1"

In your `application.conf`, add:

    mydb.driverjava="slick.driver.MySQLDriver$"
    mydb.driver="com.mysql.jdbc.Driver"
    mydb.url="jdbc:mysql://hostaddress:3306/dbname?zeroDateTimeBehavior=convertToNull"
    mydb.user="username"
    mydb.password="password"

To have a RDBMS independent architecture create an object like the following

    package mypackage
    
    import slick.driver.MySQLDriver
    import slick.driver.PostgresDriver
     
    object SlickDBDriver{
      val env = "something here"
      val driver = env match{
        case "postGreCondition" => PostgresDriver
        case _                  => MySQLDriver
      }
    }


when creating a new new model:

    import mypackage.SlickDBDriver.driver.api._
    import slick.lifted.{TableQuery, Tag}
    import slick.model.ForeignKeyAction

    case class MyModel(
      id: Option[Long],
      name: String
    ) extends Unique
    
    
    class MyModelDB(tag: Tag) extends IndexedTable[MyModel](tag, "my_table"){
      def id              = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name            = column[String]("name")
      
      def * = (id.? , name) <> ((MyModel.apply _).tupled, MyModel.unapply _)
    }

    class MyModelCrud{
       import play.api.Play.current

       val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)
       val db =  dbConfig.db

       val query = TableQuery[MyModelDB]
       
       // SELECT * FROM my_table;
       def list = db.run{query.result}
    }





## Output DDL
The whole point of using slick is to write as little SQL code as possible. After you have written your table definition, you will want to create the table in your database. 

If you have `val table = TableQuery[MyModel]` You can get the table definition (SQL code - DDL) running the following command:

    
    import mypackage.SlickDBDriver.driver.api._
    table.schema.createStatements

