---
title: "Builder Pattern"
slug: "builder-pattern"
draft: false
images: []
weight: 9890
type: docs
toc: true
---

Separates the construction of a complex object from its representation so that the same construction process can create different representations.

 - Separate the logic from representation.
 - Reuse logic to work with different set of data. 

## Java / Lombok
    import lombok.Builder;

    @Builder
    public class Email {

        private String to;
        private String from;
        private String subject;
        private String body;

    }

Usage example:

    Email.builder().to("email1@email.com")
            .from("email2@email.com")
            .subject("Email subject")
            .body("Email content")
            .build();

## Builder pattern in Java with composition
Intent:

*Separate the construction of a complex object from its representation so that the same construction process can create different representations*

Builder pattern is useful when you have few mandatory attributes and many optional attributes to construct a object. To create an object with different mandatory and optional attributes, you have to provide complex constructor to create the object.Builder pattern provides simple step-by-step process to construct a complex object. 

Real life use case:

Different users in FaceBook have different attributes, which consists of mandatory attribute like user name and optional attributes like UserBasicInfo and ContactInfo.
Some users simply provide basic info. Some users provide detailed information including Contact info. In absence of Builder pattern, you have to provide a constructor with all mandatory and optional parameters. But Builder pattern simplifies the construction process by providing simple step-by-step process to construct the complex object. 

Tips:

1. Provide a static nested builder class.
2. Provide constructor for Mandatory attributes of object.
3. Provide setter and getter methods for optional attributes of object. 
4. Return the same Builder object after setting optional attributes.
5. Provide build() method, which returns complex object


Code snippet:
   
   <!-- language: java --> 
   
    import java.util.*;
    
    class UserBasicInfo{
        String nickName;
        String birthDate;
        String gender;
        
        public UserBasicInfo(String name,String date,String gender){
            this.nickName = name;
            this.birthDate = date;
            this.gender = gender;        
        }
        
        public String toString(){
            StringBuilder sb = new StringBuilder();
            sb.append("Name:DOB:Gender:").append(nickName).append(":").append(birthDate).append(":").
            append(gender);
            return sb.toString();
        }
    }
    
    class ContactInfo{
        String eMail;
        String mobileHome;
        String mobileWork;
        
        public ContactInfo(String mail, String homeNo, String mobileOff){
            this.eMail = mail;
            this.mobileHome = homeNo;
            this.mobileWork = mobileOff;
        }    
        public String toString(){
            StringBuilder sb = new StringBuilder();
            sb.append("email:mobile(H):mobile(W):").append(eMail).append(":").append(mobileHome).append(":").append(mobileWork);
            return sb.toString();
        }
    }
    class FaceBookUser {
        String userName;
        UserBasicInfo userInfo;
        ContactInfo contactInfo;
        
        public FaceBookUser(String uName){
            this.userName = uName;
        }    
        public void setUserBasicInfo(UserBasicInfo info){
            this.userInfo = info;
        }
        public void setContactInfo(ContactInfo info){
            this.contactInfo = info;
        }    
        public String getUserName(){
            return userName;
        }
        public UserBasicInfo getUserBasicInfo(){
            return userInfo;
        }
        public ContactInfo getContactInfo(){
            return contactInfo;
        }
        
        public String toString(){
            StringBuilder sb = new StringBuilder();
            sb.append("|User|").append(userName).append("|UserInfo|").append(userInfo).append("|ContactInfo|").append(contactInfo);
            return sb.toString();
        }
        
        static class FaceBookUserBuilder{
            FaceBookUser user;
            public FaceBookUserBuilder(String userName){
                this.user = new FaceBookUser(userName);
            }
            public FaceBookUserBuilder setUserBasicInfo(UserBasicInfo info){
                user.setUserBasicInfo(info);
                return this;
            }
            public FaceBookUserBuilder setContactInfo(ContactInfo info){
                user.setContactInfo(info);
                return this;
            }
            public FaceBookUser build(){
                return user;
            }
        }
    }
    public class BuilderPattern{
        public static void main(String args[]){
            FaceBookUser fbUser1 = new FaceBookUser.FaceBookUserBuilder("Ravindra").build(); // Mandatory parameters
            UserBasicInfo info = new UserBasicInfo("sunrise","25-May-1975","M");
            
            // Build User name + Optional Basic Info 
            FaceBookUser fbUser2 = new FaceBookUser.FaceBookUserBuilder("Ravindra").
                                                    setUserBasicInfo(info).build();
            
            // Build User name + Optional Basic Info + Optional Contact Info
            ContactInfo cInfo = new ContactInfo("xxx@xyz.com","1111111111","2222222222");
            FaceBookUser fbUser3 = new FaceBookUser.FaceBookUserBuilder("Ravindra").
                                                    setUserBasicInfo(info).
                                                    setContactInfo(cInfo).build();
            
            System.out.println("Facebook user 1:"+fbUser1);
            System.out.println("Facebook user 2:"+fbUser2);
            System.out.println("Facebook user 3:"+fbUser3);
        }
    }

output:

    Facebook user 1:|User|Ravindra|UserInfo|null|ContactInfo|null
    Facebook user 2:|User|Ravindra|UserInfo|Name:DOB:Gender:sunrise:25-May-1975:M|ContactInfo|null
    Facebook user 3:|User|Ravindra|UserInfo|Name:DOB:Gender:sunrise:25-May-1975:M|ContactInfo|email:mobile(H):mobile(W):xxx@xyz.com:1111111111:2222222222


Explanation:

1. `FaceBookUser` is a complex object with below attributes using composition:  

       String userName;
       UserBasicInfo userInfo;
       ContactInfo contactInfo;

2. `FaceBookUserBuilder` is a static builder class, which contains and builds `FaceBookUser`. 

3. `userName` is only Mandatory parameter to build `FaceBookUser`
4. `FaceBookUserBuilder` builds `FaceBookUser` by setting optional parameters : `UserBasicInfo`  and `ContactInfo` 
5. This example illustrates three different FaceBookUsers with different attributes, built from Builder.
    1. fbUser1 was built as FaceBookUser with userName attribute only
    2. fbUser2 was built as FaceBookUser with userName and UserBasicInfo
    3. fbUser3 was built as FaceBookUser with userName,UserBasicInfo and ContactInfo



In above example, composition has been used instead of duplicating all attributes of FaceBookUser in Builder class. 

In creational patterns, we will first start with simple pattern like `FactoryMethod` and move towards more flexible and complex patterns like `AbstractFactory` and `Builder`.



## Builder Pattern / Java Implementation
The Builder pattern allows you to create an instance of a class with many optional variables in an easy to read way. 

Consider the following code:

<!-- language: java -->
    public class Computer {

        public GraphicsCard graphicsCard;
        public Monitor[] monitors;
        public Processor processor;
        public Memory[] ram;
        //more class variables here...
    
        Computer(GraphicsCard g, Monitor[] m, Processer p, Memory ram) {
            //code omitted for brevity...
        }

        //class methods omitted...

    }

This is all well and good if all of the parameters are necessary. What if there are a lot more variables and/or some of them are optional? You don't want to create a large number of constructors with each possible combination of required and optional parameters because it becomes difficult to maintain and for developers to understand. You also may not want to have a long list of parameters in which many may need to be entered as null by the user.

The Builder pattern creates an inner class called Builder that is used to instantiate only the desired optional variables. This is done through methods for each optional variable which take the variable type as a parameter and return a Builder object so that the methods can be chained with each other. Any required variables are put into the Builder constructor so that they can not be left out.

The Builder also includes a method called `build()` which returns the object that it is in and must be called at the end of the chain of method calls when building the object.

Following from the previous example, this code uses the Builder pattern for the Computer class. 

<!-- language: java -->
    
    public class Computer {

        private GraphicsCard graphicsCard;
        private Monitor[] monitors;
        private Processor processor;
        private Memory[] ram;
        //more class variables here...

        private Computer(Builder builder) {
            this.graphicsCard = builder.graphicsCard;
            this.monitors = builder.monitors;
            this.processor = builder.processor;
            this.ram = builder.ram;
        }

        public GraphicsCard getGraphicsCard() {
            return this.graphicsCard;
        }

        public Monitor[] getMonitors() {
            return this.monitors;
        }

        public Processor getProcessor() {
            return this.processor;
        }

        public Memory[] getRam() {
            return this.ram;
        }

        public static class Builder {
            private GraphicsCard graphicsCard;
            private Monitor[] monitors;
            private Processor processor;
            private Memory[] ram;

            public Builder(Processor p){
                this.processor = p;
            }

            public Builder graphicsCard(GraphicsCard g) {
                this.graphicsCard = g;
                return this;
            }

            public Builder monitors(Monitor[] mg) {
                this.monitors = mg;
                return this;
            }

            public Builder ram(Memory[] ram) {
                this.ram = ram;
                return this;
            }

            public Computer build() {
                return new Computer(this);
            }
        }
    }

An example of how this class would be used:
<!-- language: java -->
    public class ComputerExample {

        public static void main(String[] args) {
            Computer headlessComputer = new Computer.Builder(new Processor("Intel-i3"))
                    .graphicsCard(new GraphicsCard("GTX-960"))
                    .build();

            Computer gamingPC = new Computer.Builder(new Processor("Intel-i7-quadcode"))
                    .graphicsCard(new GraphicsCard("DX11"))
                    .monitors(new Monitor[] = {new Monitor("acer-s7"), new Monitor("acer-s7")})
                    .ram(new Memory[] = {new Memory("2GB"), new Memory("2GB"), new Memory("2GB"), new Memory("2GB")})
                    .build();
        }

    }

This example shows how the builder pattern can allow a lot of flexibility in how a class is created with fairly little effort. The Computer object can be implemented based on the callers desired configuration in an easy to read manner with little effort.

## Builder Pattern / C# / Fluent Interrface
<!-- language: c# -->
    public class Email
    {
        public string To { get; set; }
        public string From { get; set; }
        public string Subject { get; set; }
        public string Body { get; set; }
    }

    public class EmailBuilder
    {
        private readonly Email _email;

        public EmailBuilder()
        {
            _email = new Email();
        }

        public EmailBuilder To(string address)
        {
            _email.To = address;
            return this;
        }

        public EmailBuilder From(string address)
        {
            _email.From = address;
            return this;
        }

        public EmailBuilder Subject(string title)
        {
            _email.Subject = title;
            return this;
        }

        public EmailBuilder Body(string content)
        {
            _email.Body = content;
            return this;
        }

        public Email Build()
        {
            return _email;
        }
    }

Usage example:
<!-- language: c# -->
    var emailBuilder = new EmailBuilder();
    var email = emailBuilder
        .To("email1@email.com")
        .From("email2@email.com")
        .Subject("Email subject")
        .Body("Email content")
        .Build();

## Advanced Builder Pattern With Java 8 Lambda Expression
    public class Person {
    private final String salutation;
    private final String firstName;
    private final String middleName;
    private final String lastName;
    private final String suffix;
    private final Address address;
    private final boolean isFemale;
    private final boolean isEmployed;
    private final boolean isHomewOwner;

    public Person(String salutation, String firstName, String middleName, String lastName, String suffix, Address address, boolean isFemale, boolean isEmployed, boolean isHomewOwner) {
        this.salutation = salutation;
        this.firstName = firstName;
        this.middleName = middleName;
        this.lastName = lastName;
        this.suffix = suffix;
        this.address = address;
        this.isFemale = isFemale;
        this.isEmployed = isEmployed;
        this.isHomewOwner = isHomewOwner;
     }
    }

Old way

    public class PersonBuilder {
    private String salutation;
    private String firstName;
    private String middleName;
    private String lastName;
    private String suffix;
    private Address address;
    private boolean isFemale;
    private boolean isEmployed;
    private boolean isHomewOwner;

    public PersonBuilder withSalutation(String salutation) {
        this.salutation = salutation;
        return this;
    }

    public PersonBuilder withFirstName(String firstName) {
        this.firstName = firstName;
        return this;
    }

    public PersonBuilder withMiddleName(String middleName) {
        this.middleName = middleName;
        return this;
    }

    public PersonBuilder withLastName(String lastName) {
        this.lastName = lastName;
        return this;
    }

    public PersonBuilder withSuffix(String suffix) {
        this.suffix = suffix;
        return this;
    }

    public PersonBuilder withAddress(Address address) {
        this.address = address;
        return this;
    }

    public PersonBuilder withIsFemale(boolean isFemale) {
        this.isFemale = isFemale;
        return this;
    }

    public PersonBuilder withIsEmployed(boolean isEmployed) {
        this.isEmployed = isEmployed;
        return this;
    }

    public PersonBuilder withIsHomewOwner(boolean isHomewOwner) {
        this.isHomewOwner = isHomewOwner;
        return this;
    }

    public Person createPerson() {
        return new Person(salutation, firstName, middleName, lastName, suffix, address, isFemale, isEmployed, isHomewOwner);
    }

Advanced Way:

    public class PersonBuilder {
    public String salutation;
    public String firstName;
    public String middleName;
    public String lastName;
    public String suffix;
    public Address address;
    public boolean isFemale;
    public boolean isEmployed;
    public boolean isHomewOwner;

    public PersonBuilder with(
            Consumer<PersonBuilder> builderFunction) {
        builderFunction.accept(this);
        return this;
    }


    public Person createPerson() {
        return new Person(salutation, firstName, middleName,
                lastName, suffix, address, isFemale,
                isEmployed, isHomewOwner);
    }
}

Usage:

    Person person = new PersonBuilder()
        .with($ -> {
            $.salutation = "Mr.";
            $.firstName = "John";
            $.lastName = "Doe";
            $.isFemale = false;
            $.isHomewOwner = true;
            $.address =
                new PersonBuilder.AddressBuilder()
                    .with($_address -> {
                        $_address.city = "Pune";
                        $_address.state = "MH";
                        $_address.pin = "411001";
                    }).createAddress();
        })
        .createPerson();

Refer: https://medium.com/beingprofessional/think-functional-advanced-builder-pattern-using-lambda-284714b85ed5#.d9sryx3g9

