---
title: "Lazy Loading vs Eager Loading"
slug: "lazy-loading-vs-eager-loading"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Lazy Loading vs Eager Loading
Fetching or loading data can be primarily classified into two types: eager and lazy.

In order to use Hibernate make sure you add the latest version of it to the dependencies section of your pom.xml file:

    <dependency>
        <groupId>org.hibernate</groupId>
        <artifactId>hibernate-core</artifactId>   
        <version>5.2.1.Final</version>
    </dependency>


**1. Eager Loading And Lazy Loading**

The first thing that we should discuss here is what lazy loading and eager loading are:

Eager Loading is a design pattern in which data initialization occurs on the spot.
It means that collections are fetched fully at the time their parent is fetched (fetch immediately)

Lazy Loading is a design pattern which is used to defer initialization of an object until the point at which it is needed. This can effectively contribute to application's performance.

**2. Using The Different Types Of Loading**

Lazy loading can be enabled using the following XML parameter:

    lazy="true"

Let's delve into the example. First we have a User class:

    public class User implements Serializable {
       
        private Long userId;
        private String userName;
        private String firstName;
        private String lastName;
        private Set<OrderDetail> orderDetail = new HashSet<>();
    
        //setters and getters
        //equals and hashcode
        }

Look at the Set of orderDetail that we have. Now let's have a look at the **OrderDetail class**:

    public class OrderDetail implements Serializable {
    
        private Long orderId;
        private Date orderDate;
        private String orderDesc;
        private User user;
    
        //setters and getters
        //equals and hashcode
    }

The important part that is involved in setting the lazy loading in the `UserLazy.hbm.xml`:

    <set name="orderDetail" table="USER_ORDER" inverse="true" lazy="true" fetch="select">
        <key>
            <column name="USER_ID" not-null="true" />
        </key>
       <one-to-many class="com.baeldung.hibernate.fetching.model.OrderDetail" />
    </set>

This is how the lazy loading is enabled. To disable lazy loading we can simply use: `lazy = "false"` and this in turn will enable eager loading. The following is the example of setting up eager loading in another file User.hbm.xml:

    <set name="orderDetail" table="USER_ORDER" inverse="true" lazy="false" fetch="select">
        <key>
            <column name="USER_ID" not-null="true" />
        </key>
       <one-to-many class="com.baeldung.hibernate.fetching.model.OrderDetail" />
    </set>




## Scope
For those who haven't played with these two designs, the scope of lazy and eager is within a specific **Session** of *SessionFactory*. *Eager* loads everything instantly, means there is no need to call anything for fetching it. But lazy fetch usually demands some action to retrieve mapped collection/object. This sometimes is problematic getting lazy fetch outside the *session*. 
For instance, you have a view which shows the detail of the some mapped POJO.
    
    @Entity
    public class User {
        private int userId;
        private String username;
        @OneToMany
        private Set<Page> likedPage;

        // getters and setters here
    }

    @Entity
    public class Page{
        private int pageId;
        private String pageURL;

        // getters and setters here
    }

    public class LazzyTest{
        public static void main(String...s){
            SessionFactory sessionFactory = new SessionFactory();
            Session session = sessionFactory.openSession();
            Transaction transaction = session.beginTransaction();
            
            User user = session.get(User.class, 1);
            transaction.commit();
            session.close();
            
            // here comes the lazy fetch issue
            user.getLikedPage();
        }
    }

When you will try to get **lazy fetched** outside the *session* you will get the *[lazyinitializeException][1]*. This is because by default fetch strategy for all oneToMany or any other relation is *lazy*(call to DB on demand) and when you have closed the session, you have no power to communicate with database. so our code tries to fetch collection of *likedPage* and it throws exception because there is no associated session for rendering DB.

Solution for this is to use:

 1. [Open Session in View][2] - In which you keep the session open even on the rendered view.
 2. `Hibernate.initialize(user.getLikedPage())` before closing session - This tells hibernate to initialize the collection elements


  [1]: https://docs.jboss.org/hibernate/orm/3.5/javadocs/org/hibernate/LazyInitializationException.html
  [2]: https://dzone.com/articles/open-session-view-design

