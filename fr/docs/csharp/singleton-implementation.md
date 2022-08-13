---
title: "Implémentation singleton"
slug: "implementation-singleton"
draft: false
images: []
weight: 9710
type: docs
toc: true
---

## Singleton initialisé statiquement
    public class Singleton
    {
        private readonly static Singleton instance = new Singleton();
        private Singleton() { }
        public static Singleton Instance => instance;
    }

Cette implémentation est thread-safe car dans ce cas l'objet `instance` est initialisé dans le constructeur statique. Le CLR garantit déjà que tous les constructeurs statiques sont exécutés en toute sécurité.

Muter `instance` n'est pas une opération thread-safe, donc l'attribut `readonly` garantit l'immuabilité après l'initialisation.

## Singleton paresseux et thread-safe (utilisant Lazy<T>)
Le type .Net 4.0 Lazy<T> garantit une initialisation d'objet thread-safe, ce type pourrait donc être utilisé pour créer des Singletons.


    public class LazySingleton
    {
        private static readonly Lazy<LazySingleton> _instance =
            new Lazy<LazySingleton>(() => new LazySingleton());
     
        public static LazySingleton Instance
        {
            get { return _instance.Value; }
        }

        private LazySingleton() { }
    }

L'utilisation de `Lazy<T>` garantira que l'objet n'est instancié que lorsqu'il est utilisé quelque part dans le code appelant.

Une utilisation simple ressemblera à :

    using System;
                        
    public class Program
    {
        public static void Main()
        {
            var instance = LazySingleton.Instance;
        }
    }

[Démo en direct sur .NET Fiddle][1]

[1] : https://dotnetfiddle.net/oHVpK3

## Singleton paresseux et thread-safe (utilisant le verrouillage à double vérification)
Cette version thread-safe d'un singleton était nécessaire dans les premières versions de .NET où l'initialisation "statique" n'était pas garantie pour être thread-safe. Dans les versions plus modernes du framework, un [singleton initialisé statiquement] (https://www.wikiod.com/fr/docs/c%23/1192/singleton-implementation/3863/statically-initialized-singleton) est généralement préféré car il est très facile faire des erreurs d'implémentation dans le modèle suivant.

    public sealed class ThreadSafeSingleton
    {
       private static volatile ThreadSafeSingleton instance;
       private static object lockObject = new Object();
    
       private ThreadSafeSingleton()
       {
       }
    
       public static ThreadSafeSingleton Instance
       {
          get 
          {
             if (instance == null) 
             {
                lock (lockObject) 
                {
                   if (instance == null)
                   {
                      instance = new ThreadSafeSingleton();
                   }
                }
             }
    
             return instance;
          }
       }
    }

Notez que la vérification `if (instance == null)` est effectuée deux fois : une fois avant l'acquisition du verrou et une fois après. Cette implémentation serait toujours thread-safe même sans la première vérification null. Cependant, cela signifierait qu'un verrou serait acquis * chaque fois * que l'instance est demandée, ce qui entraînerait une baisse des performances. La première vérification nulle est ajoutée afin que le verrou ne soit acquis que si cela est nécessaire. La deuxième vérification nulle s'assure que seul le premier thread à acquérir le verrou crée ensuite l'instance. Les autres threads trouveront l'instance à remplir et passeront à autre chose.

## Singleton paresseux et thread-safe (pour .NET 3.5 ou version antérieure, implémentation alternative)
Étant donné que dans .NET 3.5 et versions antérieures, vous n'avez pas de classe [`Lazy<T>`][1], vous utilisez le modèle suivant :

    public class Singleton
    {
        private Singleton() // prevents public instantiation
        {
        }
    
        public static Singleton Instance
        {
            get
            {
                return Nested.instance;
            }
        }
        
        private class Nested
        {
            // Explicit static constructor to tell C# compiler
            // not to mark type as beforefieldinit
            static Nested()
            {
            }
    
            internal static readonly Singleton instance = new Singleton();
        }
    }

Ceci est inspiré du [article de blog de Jon Skeet] [2].

Étant donné que la classe `Nested` est imbriquée et privée, l'instanciation de l'instance singleton ne sera pas déclenchée par l'accès à d'autres membres de la classe `Sigleton` (comme une propriété publique en lecture seule, par exemple).


[1] : https://msdn.microsoft.com/en-us/library/dd642331(v=vs.110).aspx
[2] : http://www.yoda.arachsys.com/csharp/singleton.html

## Suppression de l'instance Singleton lorsqu'elle n'est plus nécessaire
La plupart des exemples montrent l'instanciation et le maintien d'un objet `LazySingleton` jusqu'à ce que l'application propriétaire se termine, même si cet objet n'est plus nécessaire à l'application. Une solution consiste à implémenter `IDisposable` et à définir l'instance d'objet sur null comme suit :

    public class LazySingleton : IDisposable
    {
        private static volatile Lazy<LazySingleton> _instance;
        private static volatile int _instanceCount = 0;
        private bool _alreadyDisposed = false;
 
    public static LazySingleton Instance
    {
        get
        {
            if (_instance == null)
                _instance = new Lazy<LazySingleton>(() => new LazySingleton());
            _instanceCount++;
            return _instance.Value;
        }
    }

    private LazySingleton() { }

    // Public implementation of Dispose pattern callable by consumers.
    public void Dispose()
    { 
        if (--_instanceCount == 0) // No more references to this object.
        {       
           Dispose(true);
           GC.SuppressFinalize(this);           
        }
    }
   
    // Protected implementation of Dispose pattern.
    protected virtual void Dispose(bool disposing)
    {
        if (_alreadyDisposed) return; 
      
        if (disposing) 
        {
            _instance = null; // Allow GC to dispose of this instance.
            // Free any other managed objects here.
        }
      
        // Free any unmanaged objects here.
        _alreadyDisposed = true;
    }

Le code ci-dessus supprime l'instance avant l'arrêt de l'application, mais uniquement si les consommateurs appellent `Dispose()` sur l'objet après chaque utilisation. Puisqu'il n'y a aucune garantie que cela se produise ou un moyen de le forcer, il n'y a également aucune garantie que l'instance sera un jour supprimée. Mais si cette classe est utilisée en interne, il est plus facile de s'assurer que la méthode `Dispose()` est appelée après chaque utilisation. Un exemple suit :

    public class Program
    {
        public static void Main()
        {
            using (var instance = LazySingleton.Instance)
            {
                // Do work with instance
            }
        }
    }

Veuillez noter que cet exemple n'est **pas thread-safe**.

