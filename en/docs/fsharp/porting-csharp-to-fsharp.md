---
title: "Porting C# to F#"
slug: "porting-c-to-f"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Class Implementing an Interface
Classes implement an interface to meet the interface's contract. For example, a C# class may implement `IDisposable`, 

    public class Resource : IDisposable
    { 
        private MustBeDisposed internalResource;
        
        public Resource() 
        { 
            internalResource = new MustBeDisposed();
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing){
            if (disposing){
                if (resource != null) internalResource.Dispose();
            }
        }
    }

To implement an interface in F#, use `interface` in the type definition, 

    type Resource() = 
        let internalResource = new MustBeDisposed()

        interface IDisposable with
            member this.Dispose(): unit = 
                this.Dispose(true)
                GC.SuppressFinalize(this)
    
        member __.Dispose disposing = 
            match disposing with 
            | true  -> if (not << isNull) internalResource then internalResource.Dispose()
            | false -> ()

## POCOs
Some of the simplest kinds of classes are POCOs. 

    // C#
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public DateTime Birthday { get; set; }
    }

In F# 3.0, auto-properties similar to C# auto-properties were introduced, 

    // F#
    type Person() = 
        member val FirstName = "" with get, set
        member val LastName = "" with get, set
        member val BirthDay = System.DateTime.Today with get, set

Creation of an instance of either is similar, 

    // C#
    var person = new Person { FirstName = "Bob", LastName = "Smith", Birthday = DateTime.Today }; 
    // F#
    let person = new Person(FirstName = "Bob", LastName = "Smith")

---

If you can use immutable values, a record type is much more idiomatic F#. 

    type Person = { 
        FirstName:string; 
        LastName:string; 
        Birthday:System.DateTime 
    } 

And this record can be created: 

    let person = { FirstName = "Bob"; LastName = "Smith"; Birthday = System.DateTime.Today }

Records can also be created based on other records by specifiying the existing record and adding `with`, then a list of fields to override:

    let formal = { person with FirstName = "Robert" }

