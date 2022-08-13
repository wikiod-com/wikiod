---
title: "Serialization"
slug: "serialization"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Serialization in WCF
> Serialization is the process of converting an object into a stream of bytes in order to store the object or transmit it to memory, a database, or a file.
[Microsoft page Serialization][1]

The following example demonstrates Serialization in WCF:

    [ServiceContract(Namespace="http://Microsoft.ServiceModel.Samples")]   
    public interface IPerson 
    {

        [OperationContract]
        void Add(Person person);
    
        [DataContract]
        public class Person
        {
            private int id;
    
            [DataMember]
            public int Age{ set; get;}
        }
    }

1. `[DataContract]` Attribute is used with the classes. Here it is decorated with `Person` class.

2. `[OperationContract]` is used for methods. Here it is decorated with `Add` method.

3. `[DataMember]` Attribute is used with the properties. those who are decorated with `[DataMember]` Attributes only those will be available for the proxy to access. Here we have 2 properties in that `id` is not accessible and `Age` is accessible.  

4. `[DataMember]` Attribute is handy when you don't want to show private fields to outside world and only want to show public properties.

5. With `[DataMember]` Attribute you have some properties stick to it. they are as follows
     
     

> **Properties of DataMember**

   a. `IsRequired`can be used like this `[DataMember(IsRequired=true)]` 

   b. `Name` can be used like this `[DataMember(Name=“RegistrationNo”)]`

   c. `order` can be used like this `[DataMember(order=1)]`  

Without specifying attributes, we won't be able to access the class/ method/ property in projects whom we work with (this exmple wcf service interface).

The way these attributes make the code accessible through individual projects at runtime is called "Serialization".


* With WCF you can communicate with other projects, applications or any other software using serialization, **without** all the work of setting up the endpoints, creating streams manually and maintaining them. Not to mention converting all of the data into bytes and vice versa.

  [1]: https://msdn.microsoft.com/en-us/library/mt656716.aspx

