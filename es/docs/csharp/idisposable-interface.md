---
title: "Interfaz desechable"
slug: "interfaz-desechable"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

- Depende de los clientes de la clase que implementan `IDisposable` asegurarse de llamar al método `Dispose` cuando terminen de usar el objeto. No hay nada en CLR que busque objetos directamente para invocar un método `Dispose`.

- No es necesario implementar un finalizador si su objeto solo contiene recursos administrados. Asegúrese de llamar a `Dispose` en todos los objetos que usa su clase cuando implementa su propio método `Dispose`.

- Se recomienda hacer que la clase sea segura contra múltiples llamadas a `Dispose`, aunque idealmente debería llamarse solo una vez. Esto se puede lograr agregando una variable `private bool` a su clase y estableciendo el valor en `true` cuando se haya ejecutado el método `Dispose`.

## En una clase que contiene solo recursos administrados
Los recursos administrados son recursos que el recolector de elementos no utilizados del tiempo de ejecución conoce y controla. Hay muchas clases disponibles en la BCL, por ejemplo, como `SqlConnection` que es una clase contenedora para un recurso no administrado. Estas clases ya implementan la interfaz `IDisposable`; depende de su código limpiarlas cuando haya terminado.

No es necesario implementar un finalizador si su clase solo contiene recursos administrados.

    public class ObjectWithManagedResourcesOnly : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();

        public void Dispose()
        {
            sqlConnection.Dispose();
        }
    }

## En una clase con recursos administrados y no administrados
Es importante dejar que la finalización ignore los recursos administrados. El finalizador se ejecuta en otro subproceso; es posible que los objetos administrados ya no existan cuando se ejecuta el finalizador. La implementación de un método `Dispose(bool)` protegido es una práctica común para garantizar que los recursos administrados no tengan su método `Dispose` llamado desde un finalizador.

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

## IDesechable, Desechar


## usando palabra clave
Cuando un objeto implementa la interfaz `IDisposable`, se puede crear dentro de la sintaxis `using`:

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

[Ver demostración][1]

`using` es [azúcar sintático][2] para un bloque `try/finally`; el uso anterior se traduciría aproximadamente en:

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

## En una clase heredada con recursos administrados
Es bastante común que pueda crear una clase que implemente `IDisposable` y luego derivar clases que también contienen recursos administrados. Se recomienda marcar el método `Dispose` con la palabra clave `virtual` para que los clientes tengan la capacidad de limpiar cualquier recurso que puedan poseer.

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

