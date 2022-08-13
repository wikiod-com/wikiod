---
title: "Spring boot + Hibernate + Web UI (Thymeleaf)"
slug: "spring-boot-+-hibernate-+-web-ui-thymeleaf"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

This thread is focused on how to create a spring boot application with hibernate and thymyleaf template engine.

Also check the [Thymeleaf documentation](https://www.wikiod.com/thymeleaf)

## Hibernate Configuration
First, lets overview what we need in order to setup Hibernate correctly.

1) `@EnableTransactionManagement` and `@EnableJpaRepositories` - we want transactional management and to use spring data repositories.
2) `DataSource` - main datasource for the application. using in-memory h2 for this example.
3)  `LocalContainerEntityManagerFactoryBean` - spring entity manager factory using `HibernateJpaVendorAdapter`.
4) `PlatformTransactionManager` - main transaction manager for `@Transactional` annotated components.

Configuration file:

    @Configuration
    @EnableTransactionManagement
    @EnableJpaRepositories(basePackages = "com.example.repositories")
    public class PersistanceJpaConfig {
        
        @Bean
        public DataSource dataSource() {
            DriverManagerDataSource dataSource = new DriverManagerDataSource();
            dataSource.setDriverClassName("org.h2.Driver");
            dataSource.setUrl("jdbc:h2:mem:testdb;mode=MySQL;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE");
            dataSource.setUsername("sa");
            dataSource.setPassword("");
            return dataSource;
        }
        
        @Bean
        public LocalContainerEntityManagerFactoryBean entityManagerFactory(DataSource dataSource) {
            LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean();
            em.setDataSource(dataSource);
            em.setPackagesToScan(new String[] { "com.example.models" });
            JpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
            em.setJpaVendorAdapter(vendorAdapter);
            em.setJpaProperties(additionalProperties());
            return em;
        }
    
        @Bean
        public PlatformTransactionManager transactionManager(LocalContainerEntityManagerFactoryBean entityManagerFactory, DataSource dataSource) {
            JpaTransactionManager tm = new JpaTransactionManager();
            tm.setEntityManagerFactory(entityManagerFactory.getObject());
            tm.setDataSource(dataSource);
            return tm;
        }
    
        Properties additionalProperties() {
            Properties properties = new Properties();
            properties.setProperty("hibernate.hbm2ddl.auto", "update");
            properties.setProperty("hibernate.dialect", "org.hibernate.dialect.H2Dialect");
            return properties;
        }
    
    }

## Entities and Repositories
A simple entity: Using Lombok `@Getter` and `@Setter` annotations to generate getters and setters for us

    @Entity
    @Getter @Setter
    public class Message {
        
        @Id
        @GeneratedValue(generator = "system-uuid")
        @GenericGenerator(name = "system-uuid", strategy = "uuid")
        private String id;
        private String message;
    
    }

I am using UUID based ids and lombok to generate getters and setters.

A simple repository for the entity above:

    @Transactional
    public interface MessageRepository extends CrudRepository<Message, String> {
    }

More on respositories: [spring data docs][1]

Make sure entities reside in a package that is mapped in `em.setPackagesToScan` (defined in `LocalContainerEntityManagerFactoryBean` bean) and repositories in a package mapped in `basePackages` (defined in `@EnableJpaRepositories` annotation)

  [1]: https://docs.spring.io/spring-data/jpa/docs/current/reference/html/#repositories.core-concepts

## Maven dependencies
This example is based on spring boot 1.5.1.RELEASE. with the following dependencies:

    <!-- Spring -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-thymeleaf</artifactId>
    </dependency>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>
    <!-- Lombok -->
    <dependency>
        <groupId>org.projectlombok</groupId>
        <artifactId>lombok</artifactId>
        <optional>true</optional>
    </dependency>
    <!-- H2 -->
    <dependency>
        <groupId>com.h2database</groupId>
        <artifactId>h2</artifactId>
    </dependency>
    <!-- Test -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-test</artifactId>
        <scope>test</scope>
    </dependency>

In this example we are going to use Spring Boot JPA, Thymeleaf and web starters.
I am using Lombok to generate getters and setters easier but it is not mandatory.
H2 will be used as an in-memory easy to configure database.


## Thymeleaf Resources and Spring Controller
In order to expose Thymeleaf templates we need to define controllers.

Example:

    @Controller
    @RequestMapping("/")
    public class MessageController {
        
        @Autowired
        private MessageRepository messageRepository;
        
        @GetMapping
        public ModelAndView index() {
            Iterable<Message> messages = messageRepository.findAll();
            return new ModelAndView("index", "index", messages);
        }
        
    }

This simple controller injects `MessageRepository` and pass all messages to a template file named `index.html`, residing in `src/main/resources/templates`, and finally expose it on `/index`.

In the same way, we can place other templates in the templates folder (default by spring to `src/main/resources/templates`), pass a model to them and serve them to the client.

Other static resources should be placed in one of the following folders, exposed by default in spring boot:

    /META-INF/resources/
    /resources/
    /static/
    /public/

Thymeleaf `index.html` example:

    <!DOCTYPE html>
    <html xmlns:th="http://www.thymeleaf.org">
        <head th:fragment="head (title)">
            <title th:text="${title}">Index</title>
            <link rel="stylesheet" th:href="@{/css/bootstrap.min.css}" href="../../css/bootstrap.min.css" />
        </head>
        <body>
            <nav class="navbar navbar-default navbar-fixed-top">
              <div class="container-fluid">
                <div class="navbar-header">
                  <a class="navbar-brand" href="#">Thymeleaf</a>
                </div>
              </div>
            </nav>
            <div class="container">
                <ul class="nav">
                    <li><a th:href="@{/}" href="messages.html"> Messages </a></li>
                </ul>
            </div>
        </body>
    </html>

* `bootstrap.min.css` is in `src/main/resources/static/css` folder. you can use the syntax `@{}` to get other static resources using relative path. 

