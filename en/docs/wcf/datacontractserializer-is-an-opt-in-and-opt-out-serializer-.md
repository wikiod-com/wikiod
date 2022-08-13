---
title: "DataContractSerializer is an Opt-In and Opt-Out serializer."
slug: "datacontractserializer-is-an-opt-in-and-opt-out-serializer"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

actually its so simple: Opt-In approach says properties that are considered to be part of DataContract must be explicitly marked otherwise will be ignore, while Opt-Out means all of the properties will be assumed to be part of the DataContract unless marked explicitly.

## What is opt in serializer

    /// <summary>
    /// Defines a student.
    /// </summary>
    [DataContract]
    public class Student
    {
        /// <summary>
        /// Gets or sets the student number.
        /// </summary>
        [DataMember]
        public string StudentNumber { get; set; }

        /// <summary>
        /// Gets or sets the first name.
        /// </summary>
        [DataMember]
        public string FirstName { get; set; }

        /// <summary>
        /// Gets or sets the last name.
        /// </summary>
        [DataMember]
        public string LastName { get; set; }

        /// <summary>
        /// Gets or sets the marks obtained.
        /// </summary>
        public int MarksObtained { get; set; }
    }

    /// <summary>
    /// A service that provides the operations for students.
    /// </summary>
    [ServiceContract]
    public interface IStudentService
    {
        //Service contract code here.
    }

In above code `StudentNumber`, `FirstName`, `LastName` properties of `Student` class are explicitly marked with `DataMember` attribute as oppose to `MarksObtained`, so `MarksObtained` will be ignored. From ignored it means that `MarksObtained` wont be the part of data going across the wire to / from this service.

## What is opt out serializer
Below code represents an example of Opt-Out approach using `Serializable` and `NonSerialized` attributes.

    /// <summary>
    /// Represents a student.
    /// </summary>
    [Serializable]
    public class Student
    {
        /// <summary>
        /// Gets or sets student number.
        /// </summary>
        public string StudentNumber { get; set; }
    
        /// <summary>
        /// Gets or sets first name.
        /// </summary>
        public string FirstName { get; set; }
    
        /// <summary>
        /// Gets or sets last name.
        /// </summary>
        public string LastName { get; set; }
    
        /// <summary>
        /// Gets or sets marks obtained.
        /// </summary>
        [NonSerialized]
        public string MarksObtained { get; set; }
    }
    
    /// <summary>
    /// A service that provides the operations for student.
    /// </summary>
    [ServiceContract]
    public interface IStudentService
    {
        //Service contract code here. Example given.
        
        /// <summary>
        /// Adds a student into the system.
        /// </summary>
        /// <param name="student">Student to be added.</param>
        [OperationContract]
        void AddStudent(Student student);
    }

In above example, we explicitly marked `MarksObtained` property as `[NonSerialized]` attribute, so it will be ignored except the others. Hope this helps!

