---
title: "Interface jetable"
slug: "interface-jetable"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

- C'est aux clients de la classe implémentant `IDisposable` de s'assurer qu'ils appellent la méthode `Dispose` lorsqu'ils ont fini d'utiliser l'objet. Il n'y a rien dans le CLR qui recherche directement dans les objets une méthode "Dispose" à invoquer.

- Il n'est pas nécessaire d'implémenter un finaliseur si votre objet ne contient que des ressources gérées. Assurez-vous d'appeler `Dispose` sur tous les objets que votre classe utilise lorsque vous implémentez votre propre méthode `Dispose`.

- Il est recommandé de protéger la classe contre les appels multiples à `Dispose`, bien qu'elle ne doive idéalement être appelée qu'une seule fois. Cela peut être réalisé en ajoutant une variable `private bool` à votre classe et en définissant la valeur sur `true` lorsque la méthode `Dispose` a été exécutée.

## Dans une classe qui ne contient que des ressources gérées
Les ressources gérées sont des ressources que le ramasse-miettes de l'environnement d'exécution connaît et contrôle. Il existe de nombreuses classes disponibles dans la BCL, par exemple, comme une « SqlConnection » qui est une classe wrapper pour une ressource non gérée. Ces classes implémentent déjà l'interface `IDisposable` - c'est à votre code de les nettoyer lorsque vous avez terminé.

Il n'est pas nécessaire d'implémenter un finaliseur si votre classe ne contient que des ressources managées.

    public class ObjectWithManagedResourcesOnly : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();

        public void Dispose()
        {
            sqlConnection.Dispose();
        }
    }

## Dans une classe avec des ressources gérées et non gérées
Il est important de laisser la finalisation ignorer les ressources gérées. Le finaliseur s'exécute sur un autre thread -- il est possible que les objets gérés n'existent plus au moment où le finaliseur s'exécute. L'implémentation d'une méthode `Dispose(bool)` protégée est une pratique courante pour s'assurer que les ressources gérées n'ont pas leur méthode `Dispose` appelée à partir d'un finaliseur.

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


## en utilisant le mot-clé
Lorsqu'un objet implémente l'interface `IDisposable`, il peut être créé dans la syntaxe `using` :

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

[Voir la démo][1]

`using` est [syntatic sugar][2] pour un bloc `try/finally` ; l'utilisation ci-dessus se traduirait approximativement par:

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

[1] : https://dotnetfiddle.net/StEPc2
[2] : https://en.wikipedia.org/wiki/Syntactic_sugar

## Dans une classe héritée avec des ressources gérées
Il est assez courant que vous puissiez créer une classe qui implémente `IDisposable`, puis dériver des classes qui contiennent également des ressources gérées. Il est recommandé de marquer la méthode `Dispose` avec le mot-clé `virtual` afin que les clients aient la possibilité de nettoyer toutes les ressources qu'ils pourraient posséder.

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

