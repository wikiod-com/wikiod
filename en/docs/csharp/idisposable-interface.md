---
title: "IDisposable interface"
slug: "idisposable-interface"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

 - It's up to clients of the class implementing `IDisposable` to make sure they call the `Dispose` method when they are finished using the object. There is nothing in the CLR that directly searches objects for a `Dispose` method to invoke.

-  It's not necessary to implement a finalizer if your object only contains managed resources. Be sure to call `Dispose` on all of the objects that your class uses when you implement your own `Dispose` method.

- It's recommended to make the class safe against multiple calls to `Dispose`, although it should ideally be called only once. This can be achieved by adding a `private bool` variable to your class and setting the value to `true` when the `Dispose` method has run.

## In a class that contains only managed resources
Managed resources are resources that the runtime's garbage collector is aware and under control of. There are many classes available in the BCL, for example, such as a `SqlConnection` that is a wrapper class for an unmanaged resource. These classes already implement the `IDisposable` interface -- it's up to your code to clean them up when you are done.

It's not necessary to implement a finalizer if your class only contains managed resources.

    public class ObjectWithManagedResourcesOnly : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();

        public void Dispose()
        {
            sqlConnection.Dispose();
        }
    }

## In a class with managed and unmanaged resources
It's important to let finalization ignore managed resources. The finalizer runs on another thread -- it's possible that the managed objects don't exist anymore by the time the finalizer runs. Implementing a protected `Dispose(bool)` method is a common practice to ensure managed resources do not have their `Dispose` method called from a finalizer.

    public class ManagedAndUnmanagedObject : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();
        private UnmanagedHandle unmanagedHandle = Win32.SomeUnmanagedResource();
        private bool disposed;

        public void Dispose()
        {
            Dispose(true); // client called dispose
            GC.SuppressFinalize(this); // tell the GC to not execute the Finalizer
        }

        protected virtual void Dispose(bool disposeManaged)
        {
            if (!disposed)
            {
                if (disposeManaged)
                {
                    if (sqlConnection != null)
                    {
                        sqlConnection.Dispose();
                    }
                }
    
                unmanagedHandle.Release();

                disposed = true;
            }
        }

        ~ManagedAndUnmanagedObject()
        {
            Dispose(false);
        }
    }

## IDisposable, Dispose


## using keyword
When an object implements the `IDisposable` interface, it can be created within the `using` syntax:

    using (var foo = new Foo())
    {
        // do foo stuff
    } // when it reaches here foo.Dispose() will get called

    public class Foo : IDisposable
    {
        public void Dispose()
        {
            Console.WriteLine("dispose called");
        }
    }

[View demo][1]

`using` is [syntatic sugar][2] for a `try/finally` block; the above usage would  roughly translate into:

    {
        var foo = new Foo();
        try
        {
            // do foo stuff
        }
        finally
        {
            if (foo != null)
                ((IDisposable)foo).Dispose();
        }
    }

  [1]: https://dotnetfiddle.net/StEPc2
  [2]: https://en.wikipedia.org/wiki/Syntactic_sugar

## In an inherited class with managed resources
It's fairly common that you may create a class that implements `IDisposable`, and then derive classes that also contain managed resources. It is recommendeded to mark the `Dispose` method with the `virtual` keyword so that clients have the ability to cleanup any resources they may own.

    public class Parent : IDisposable
    {
        private ManagedResource parentManagedResource = new ManagedResource();

        public virtual void Dispose()
        {
            if (parentManagedResource != null)
            {
                parentManagedResource.Dispose();
            }
        }
    }

    public class Child : Parent
    {
        private ManagedResource childManagedResource = new ManagedResource();

        public override void Dispose()
        {
            if (childManagedResource != null)
            {
                childManagedResource.Dispose();
            }
            //clean up the parent's resources
            base.Dispose();
        }
    }

