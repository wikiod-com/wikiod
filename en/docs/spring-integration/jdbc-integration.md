---
title: "Jdbc Integration"
slug: "jdbc-integration"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Jdbc Inbound Adapter - xml configuration
In the [official reference document](http://docs.spring.io/spring-integration/reference/html/jdbc.html#jdbc-inbound-channel-adapter), it says:
> The main function of an inbound Channel Adapter is to execute a SQL SELECT query and turn the result set as a message. The message payload is the whole result set, expressed as a List, and the types of the items in the list depend on the row-mapping strategy that is used. The default strategy is a generic mapper that just returns a Map for each row in the query result.

[![Jdbc Inbound Adapter][1]][1]

* Source code

      public class Application {
         static class Book {
            String title;
            double price;
    
            Book(String title, double price) {
                this.title = title;
                this.price = price;
            }
    
            double getPrice() {
                return price;
            }
    
            String getTitle() {
                return title;
            }
    
            @Override
            public String toString() {
                return String.format("{title: %s, price: %s}", title, price);
            }
          }
    
          static class Consumer {
            public void consume(List<Book> books) {
                books.stream().forEach(System.out::println);
            }
          }
    
          static class BookRowMapper implements RowMapper<Book> {
    
            @Override
            public Book mapRow(ResultSet rs, int rowNum) throws SQLException {
                String title = rs.getString("TITLE");
                double price = rs.getDouble("PRICE");
                return new Book(title, price);
            }
          }
    
          public static void main(String[] args) {
            new ClassPathXmlApplicationContext(
                    "classpath:spring/integration/stackoverflow/jdbc/jdbc.xml");
          }
      }

* xml configuration file

      <?xml version="1.0" encoding="UTF-8"?>
      <beans xmlns="http://www.springframework.org/schema/beans"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xmlns:jdbc="http://www.springframework.org/schema/jdbc"
             xmlns:int="http://www.springframework.org/schema/integration"
             xmlns:int-jdbc="http://www.springframework.org/schema/integration/jdbc"
             xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
             http://www.springframework.org/schema/integration http://www.springframework.org/schema/integration/spring-integration.xsd
             http://www.springframework.org/schema/integration/jdbc http://www.springframework.org/schema/integration/jdbc/spring-integration-jdbc.xsd
             http://www.springframework.org/schema/jdbc http://www.springframework.org/schema/jdbc/spring-jdbc.xsd">
          <jdbc:embedded-database id="dataSource" type="H2">
              <jdbc:script location="classpath:spring/integration/stackoverflow/jdbc/schema.sql"/>
          </jdbc:embedded-database>
          <bean id="bookRowMapper"
                class="spring.integration.stackoverflow.jdbc.Application$BookRowMapper"/>
    
          <int:channel id="channel"/>
    
          <int-jdbc:inbound-channel-adapter id="jdbcInbound"
                                            channel="channel"
                                            data-source="dataSource"
                                            query="SELECT * FROM BOOKS"
                                            row-mapper="bookRowMapper">
              <int:poller fixed-rate="1000"/>
          </int-jdbc:inbound-channel-adapter>
    
          <int:outbound-channel-adapter id="outbound" channel="channel" method="consume">
              <bean class="spring.integration.stackoverflow.jdbc.Application$Consumer"/>
          </int:outbound-channel-adapter>
      </beans>

* schema.sql

      CREATE TABLE BOOKS (
        TITLE VARCHAR(20) NOT NULL,
        PRICE DOUBLE      NOT NULL
      );
      
      INSERT INTO BOOKS(TITLE, PRICE) VALUES('book1', 10);
      INSERT INTO BOOKS(TITLE, PRICE) VALUES('book2', 20);

* Summary:
    * `jdbcInbound`: a Jdbc inbound channel adapter. It execute the SQL `SELECT * FROM BOOKS` and convert the result set to `List<Book>` via the bean `bookRowMapper`. Finally, it send this book list to the channel `channel`.
    * `channel`: transfer the message
    * `outbound`: a generic outbound adapter. see [Generic Inbound and Outbound Channel Adapter](https://www.wikiod.com/spring-integration/getting-started-with-spring-integration#Generic Inbound and Outbound Channel Adapter)


  [1]: https://i.stack.imgur.com/49Fe0.png

## Jdbc Outbound Channel Adapter - xml configuration
In the [Spring Integration Reference Docuement](http://docs.spring.io/spring-integration/reference/html/jdbc.html#jdbc-outbound-channel-adapter), it says:
> The outbound Channel Adapter is the inverse of the inbound: its role is to handle a message and use it to execute a SQL query. The message payload and headers are available by default as input parameters to the query...

[![enter image description here][1]][1]

* Java code

      public class OutboundApplication {
          static class Book {
              String title;
              double price;
    
              Book(String title, double price) {
                  this.title = title;
                  this.price = price;
              }
    
              public double getPrice() {
                  return price;
              }
    
              public String getTitle() {
                  return title;
              }
    
          }
    
          static class Producer {
              public Book produce() {
                  return IntStream.range(0, 3)
                          .mapToObj(i -> new Book("book" + i, i * 10))
                          .collect(Collectors.toList())
                          .get(new Random().nextInt(3));
              }
          }
    
          public static void main(String[] args) {
              new ClassPathXmlApplicationContext(
                      "classpath:spring/integration/stackoverflow/jdbc/jdbc-outbound.xml");
          }
      }

* xml config file

      <?xml version="1.0" encoding="UTF-8"?>
      <beans xmlns="http://www.springframework.org/schema/beans"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xmlns:jdbc="http://www.springframework.org/schema/jdbc"
             xmlns:int="http://www.springframework.org/schema/integration"
             xmlns:int-jdbc="http://www.springframework.org/schema/integration/jdbc"
             xsi:schemaLocation="http://www.springframework.org/schema/beans
             http://www.springframework.org/schema/beans/spring-beans.xsd
             http://www.springframework.org/schema/jdbc
             http://www.springframework.org/schema/jdbc/spring-jdbc.xsd
             http://www.springframework.org/schema/integration
             http://www.springframework.org/schema/integration/spring-integration.xsd
             http://www.springframework.org/schema/integration/jdbc
             http://www.springframework.org/schema/integration/jdbc/spring-integration-jdbc.xsd">
          <bean id="dataSource" class="org.springframework.jdbc.datasource.DriverManagerDataSource">
              <property name="url" value="jdbc:h2:tcp://localhost/~/booksystem"/>
              <property name="username" value="sa"/>
              <property name="password" value=""/>
              <property name="driverClassName" value="org.h2.Driver"/>
          </bean>
          <jdbc:initialize-database>
              <jdbc:script location="classpath:spring/integration/stackoverflow/jdbc/schema.sql"/>
          </jdbc:initialize-database>
          <int:channel id="channel"/>
          <int:inbound-channel-adapter channel="channel" method="produce" >
              <bean class="spring.integration.stackoverflow.jdbc.OutboundApplication$Producer"/>
              <int:poller fixed-rate="1000"/>
          </int:inbound-channel-adapter>
          <int-jdbc:outbound-channel-adapter id="jdbcOutbound"
                                             channel="channel"
                                             data-source="dataSource"
                                             sql-parameter-source-factory="sqlParameterSource"
                                             query="INSERT INTO BOOKS(TITLE, PRICE) VALUES(:title, :price)"/>
          <bean id="sqlParameterSource"   class="org.springframework.integration.jdbc.ExpressionEvaluatingSqlParameterSourceFactory">
              <property name="parameterExpressions">
                  <map>
                      <entry key="title" value="payload.title"/>
                      <entry key="price" value="payload.price"/>
                  </map>
              </property>
          </bean>
      </beans>

* schema.sql

      DROP TABLE IF EXISTS BOOKS;
      CREATE TABLE BOOKS (
        TITLE VARCHAR(20) NOT NULL,
        PRICE DOUBLE      NOT NULL
      );

* You can observe the `BOOKS` table, and you can see the records are inserted. Or you can write a `int-jdbc:inbound-channel-adapter` to count the `BOOKS` table and you can find that the count number is growing continuously.
* Summary:
    * `inbound`: a generic inbound adapter used to get the `Book` object as message payload and send it to channel `channel`.
    * `channel`: used to transfer message.
    * `jdbcOutbound`: a jdbc outbound adapter, it receives the message with `Book` type and then prepare the query parameter `:title` and `:price` via `sqlParameterSource` bean using SpEL like `payload.title` and `payload.price` to get the title and price form the message payload.


  [1]: https://i.stack.imgur.com/J5ecg.png

