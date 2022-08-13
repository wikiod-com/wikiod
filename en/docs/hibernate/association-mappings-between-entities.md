---
title: "Association Mappings between Entities"
slug: "association-mappings-between-entities"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## OneToMany association
To illustrate relation OneToMany we need 2 Entities e.g. Country and City. One Country has multiple Cities.

In the CountryEntity beloww we define set of cities for Country.

    @Entity
    @Table(name = "Country")
    public class CountryEntity implements Serializable
    {
       private static final long serialVersionUID = 1L;
     
       @Id
       @Column(name = "COUNTRY_ID", unique = true, nullable = false)
       @GeneratedValue(strategy = GenerationType.SEQUENCE)
       private Integer           countryId;
     
       @Column(name = "COUNTRY_NAME", unique = true, nullable = false, length = 100)
       private String            countryName;
    
       @OneToMany(mappedBy="country", fetch=FetchType.LAZY)
       private Set<CityEntity> cities = new HashSet<>();
    
       //Getters and Setters are not shown
    }

Now the city entity.

    @Entity
    @Table(name = "City")
    public class CityEntity implements Serializable
    {
       private static final long serialVersionUID = 1L;
     
       @Id
       @Column(name = "CITY_ID", unique = true, nullable = false)
       @GeneratedValue(strategy = GenerationType.SEQUENCE)
       private Integer           cityId;
     
       @Column(name = "CITY_NAME", unique = false, nullable = false, length = 100)
       private String            cityName;
    
       @ManyToOne(optional=false, fetch=FetchType.EAGER)
       @JoinColumn(name="COUNTRY_ID", nullable=false)
       private CountryEntity country;
     
       //Getters and Setters are not shown
    }

## One to many association using XML
This is an example of how you would do a one to many mapping using XML. We will use Author and Book as our example and assume an author may have written many books, but each book will only have one author.

Author class:

    public class Author {
        private int id;
        private String firstName;
        private String lastName;
        
        public Author(){
            
        }
        public int getId(){
            return id;
        }
        public void setId(int id){
            this.id = id;
        }
        public String getFirstName(){
            return firstName;
        }
        public void setFirstName(String firstName){
            this.firstName = firstName;
        }
        public String getLastName(){
            return lastName;
        }
        public void setLastName(String lastName){
            this.lastName = lastName;
        }
    }

Book class:

    public class Book {
        private int id;
        private String isbn;
        private String title;    
        private Author author;
        private String publisher;
        
        public Book() {
            super();
        }
        public int getId() {
            return id;
        }
        public void setId(int id) {
            this.id = id;
        }
        public String getIsbn() {
            return isbn;
        }
        public void setIsbn(String isbn) {
            this.isbn = isbn;
        }
        public String getTitle() {
            return title;
        }
        public void setTitle(String title) {
            this.title = title;
        }
        public Author getAuthor() {
            return author;
        }
        public void setAuthor(Author author) {
            this.author = author;
        }
        public String getPublisher() {
            return publisher;
        }
        public void setPublisher(String publisher) {
            this.publisher = publisher;
        }
    }

Author.hbm.xml:

    <?xml version="1.0"?>
    <!DOCTYPE hibernate-mapping PUBLIC
        "-//Hibernate/Hibernate Mapping DTD//EN"
        "http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd" >
    
    <hibernate-mapping>
       <class name="Author" table="author">
          <meta attribute="class-description">
             This class contains the author's information. 
          </meta>
          <id name="id" type="int" column="author_id">
             <generator class="native"/>
          </id>
          <property name="firstName" column="first_name" type="string"/>
          <property name="lastName" column="last_name" type="string"/>
       </class>
    </hibernate-mapping>

Book.hbm.xml:

    <?xml version="1.0"?>
    <!DOCTYPE hibernate-mapping PUBLIC
        "-//Hibernate/Hibernate Mapping DTD//EN"
        "http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd" >
    
    <hibernate-mapping>
        <class name="Book" table="book_title">
          <meta attribute="class-description">
             This class contains the book information. 
          </meta>
            <id name="id" type="int" column="book_id">
                <generator class="native"/>
            </id>
            <property name="isbn" column="isbn" type="string"/>
            <property name="title" column="title" type="string"/>
             <many-to-one name="author" class="Author" cascade="all">
                 <column name="author"></column>
             </many-to-one>
            <property name="publisher" column="publisher" type="string"/>
        </class>
    </hibernate-mapping>

What makes the one to many connection is that the Book class contains an Author and the xml has the \<many-to-one> tag. The cascade attribute allows you to set how the child entity will be saved/updated.

