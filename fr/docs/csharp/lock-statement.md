---
title: "Déclaration de verrouillage"
slug: "declaration-de-verrouillage"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Syntaxe
* verrouiller (obj) {}

En utilisant l'instruction `lock`, vous pouvez contrôler l'accès de différents threads au code dans le bloc de code. Il est couramment utilisé pour empêcher les conditions de concurrence, par exemple plusieurs threads lisant et supprimant des éléments d'une collection. Comme le verrouillage oblige les threads à attendre que d'autres threads quittent un bloc de code, cela peut entraîner des retards qui pourraient être résolus avec d'autres méthodes de synchronisation.

MSDN

> Le mot-clé lock marque un bloc d'instructions comme une section critique en
> obtenir le verrou d'exclusion mutuelle pour un objet donné, exécuter une
> déclaration, puis relâchez le verrou.
> 
> Le mot-clé lock garantit qu'un thread n'entre pas dans un
> section de code alors qu'un autre thread se trouve dans la section critique. Si
> un autre thread essaie d'entrer un code verrouillé, il va attendre, bloquer,
> jusqu'à ce que l'objet soit libéré.
> 
> La meilleure pratique consiste à définir un objet **privé** à verrouiller, ou un objet **privé
> variable objet statique** pour protéger les données communes à toutes les instances.

<h>

En C# 5.0 et versions ultérieures, l'instruction `lock` équivaut à :

    bool lockTaken = false;
    try 
    {
        System.Threading.Monitor.Enter(refObject, ref lockTaken);
        // code 
    }
    finally 
    {
        if (lockTaken)
            System.Threading.Monitor.Exit(refObject);
    }

Pour C# 4.0 et versions antérieures, l'instruction `lock` équivaut à :

    System.Threading.Monitor.Enter(refObject);
    try 
    {
        // code
    }
    finally 
    {
         System.Threading.Monitor.Exit(refObject);
    }

## Lancer une exception dans une instruction de verrouillage
Le code suivant libérera le verrou. Il n'y aura aucun problème. L'instruction de verrouillage des coulisses fonctionne comme "essayez enfin"

    lock(locker)
    {
        throw new Exception();
    }

Vous pouvez en voir plus dans la [Spécification C# 5.0][1] :

Une instruction `lock` de la forme

    lock (x) ...

où `x` est une expression d'un *type de référence*, est précisément équivalent à
    
    bool __lockWasTaken = false;
    try {
        System.Threading.Monitor.Enter(x, ref __lockWasTaken);
        ...
    }
    finally {
        if (__lockWasTaken) System.Threading.Monitor.Exit(x);
    }

sauf que `x` n'est évalué qu'une seule fois.


[1] : https://msdn.microsoft.com/en-us/library/aa664735%28VS.71%29.aspx?f=255&MSPPError=-2147217396

## Utilisation simple
L'utilisation courante de `lock` est une section critique.

Dans l'exemple suivant, `ReserveRoom` est censé être appelé à partir de différents threads. La synchronisation avec `lock` est le moyen le plus simple d'éviter les conditions de concurrence ici. Le corps de la méthode est entouré de `lock` qui garantit que deux threads ou plus ne peuvent pas l'exécuter simultanément.
 
    public class Hotel
    {
        private readonly object _roomLock = new object();

        public void ReserveRoom(int roomNumber)
        {
            // lock keyword ensures that only one thread executes critical section at once
            // in this case, reserves a hotel room of given number
            // preventing double bookings
            lock (_roomLock)
            {
                // reserve room logic goes here
            }
        }
    }

Si un thread atteint un bloc `lock` alors qu'un autre thread s'y exécute, le premier en attendra un autre pour quitter le bloc.

> La meilleure pratique consiste à définir un objet privé à verrouiller ou un
> variable objet statique pour protéger les données communes à toutes les instances.

## Retour dans une instruction de verrouillage
Le code suivant libérera le verrou.

    lock(locker)
    {
        return 5;
    }

Pour une explication détaillée, [cette réponse SO][1] est recommandée.


[1] : http://stackoverflow.com/a/266718/1519458

## Anti-Patterns et pièges
# Verrouillage sur une variable allouée par la pile / locale

L'une des erreurs lors de l'utilisation de `lock` est l'utilisation d'objets locaux comme verrou dans une fonction. Étant donné que ces instances d'objets locaux différeront à chaque appel de la fonction, `lock` ne fonctionnera pas comme prévu.

    List<string> stringList = new List<string>();

    public void AddToListNotThreadSafe(string something)
    {
        // DO NOT do this, as each call to this method 
        // will lock on a different instance of an Object.
        // This provides no thread safety, it only degrades performance.
        var localLock = new Object();
        lock(localLock)
        {
            stringList.Add(something);
        }
    }

    // Define object that can be used for thread safety in the AddToList method
    readonly object classLock = new object();

    public void AddToList(List<string> stringList, string something)
    {
        // USE THE classLock instance field to achieve a 
        // thread-safe lock before adding to stringList
        lock(classLock)
        {
            stringList.Add(something);
        }
    }

# En supposant que le verrouillage restreint l'accès à l'objet de synchronisation lui-même

Si un thread appelle : `lock(obj)` et qu'un autre thread appelle `obj.ToString()`, le deuxième thread ne sera pas bloqué.

    object obj = new Object();
     
    public void SomeMethod()
    {
         lock(obj)
        {
           //do dangerous stuff 
        }
     }

     //Meanwhile on other tread 
     public void SomeOtherMethod()
     {
       var objInString = obj.ToString(); //this does not block
     }

# S'attendre à ce que les sous-classes sachent quand verrouiller

Parfois, les classes de base sont conçues de telle sorte que leurs sous-classes doivent utiliser un verrou lors de l'accès à certains champs protégés :

    public abstract class Base
    {
        protected readonly object padlock;
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public abstract void Do();
    }

    public class Derived1 : Base
    {
        public override void Do()
        {
            lock (this.padlock)
            {
                this.list.Add("Derived1");
            }
        }
    }

    public class Derived2 : Base
    {
        public override void Do()
        {
            this.list.Add("Derived2"); // OOPS! I forgot to lock!
        }
    }

Il est beaucoup plus sûr d'*encapsuler le verrouillage* en utilisant une [méthode de modèle][3] :

    public abstract class Base
    {
        private readonly object padlock; // This is now private
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public void Do()
        {
            lock (this.padlock) {
                this.DoInternal();
            }
        }

        protected abstract void DoInternal();
    }

    public class Derived1 : Base
    {
        protected override void DoInternal()
        {
            this.list.Add("Derived1"); // Yay! No need to lock
        }
    }

# Le verrouillage sur une variable ValueType encadrée ne se synchronise pas

Dans l'exemple suivant, une variable privée est implicitement encadrée car elle est fournie en tant qu'argument "object" à une fonction, s'attendant à ce qu'une ressource de moniteur se verrouille.
Le boxing se produit juste avant l'appel de la fonction IncInSync, de sorte que l'instance boxed correspond à un objet de tas différent chaque fois que la fonction est appelée.

    public int Count { get; private set; }

    private readonly int counterLock = 1;
    
    public void Inc()
    {
        IncInSync(counterLock);
    }

    private void IncInSync(object monitorResource)
    {
        lock (monitorResource)
        {
            Count++;
        }
    }

La boxe se produit dans la fonction `Inc` :

    BulemicCounter.Inc:
    IL_0000:  nop         
    IL_0001:  ldarg.0     
    IL_0002:  ldarg.0     
    IL_0003:  ldfld       UserQuery+BulemicCounter.counterLock
    IL_0008:  box         System.Int32**
    IL_000D:  call        UserQuery+BulemicCounter.IncInSync
    IL_0012:  nop         
    IL_0013:  ret         

Cela ne signifie pas qu'un ValueType encadré ne peut pas du tout être utilisé pour le verrouillage du moniteur :

    private readonly object counterLock = 1;

Maintenant, la boxe se produit dans le constructeur, ce qui est bien pour le verrouillage :

    IL_0001:  ldc.i4.1    
    IL_0002:  box         System.Int32
    IL_0007:  stfld       UserQuery+BulemicCounter.counterLock

# Utiliser des verrous inutilement lorsqu'une alternative plus sûre existe

Un modèle très courant consiste à utiliser une "Liste" ou un "Dictionnaire" privé dans une classe thread-safe et à la verrouiller à chaque accès :

    public class Cache
    {
        private readonly object padlock;
        private readonly Dictionary<string, object> values;

        public WordStats()
        {
            this.padlock = new object();
            this.values = new Dictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            lock (this.padlock)
            {
                this.values.Add(key, value);
            }
        }

        /* rest of class omitted */
    }

S'il existe plusieurs méthodes accédant au dictionnaire `values`, le code peut devenir très long et, plus important encore, le verrouillage en permanence obscurcit son *intention*. Le verrouillage est également très facile à oublier et le manque de verrouillage approprié peut rendre les bogues très difficiles à trouver.

En utilisant un [`ConcurrentDictionary`][1], nous pouvons éviter de verrouiller complètement :

    public class Cache
    {
        private readonly ConcurrentDictionary<string, object> values;

        public WordStats()
        {
            this.values = new ConcurrentDictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            this.values.Add(key, value);
        }

        /* rest of class omitted */
    }

L'utilisation de collections simultanées améliore également les performances car [elles utilisent toutes des techniques sans verrou] [2] dans une certaine mesure.

[1] : https://msdn.microsoft.com/en-us/library/dd287191%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[2] : https://blogs.msdn.microsoft.com/pfxteam/2010/01/26/faq-are-all-of-the-new-concurrent-collections-lock-free/
[3] : https://en.wikipedia.org/wiki/Template_method_pattern

## Utilisation d'instances d'Objet pour verrouiller
Lors de l'utilisation de l'instruction `lock` intégrée de C #, une instance d'un certain type est nécessaire, mais son état n'a pas d'importance. Une instance de `object` est parfaite pour cela :

    public class ThreadSafe {
      private static readonly object locker = new object();


      public void SomeThreadSafeMethod() {
        lock (locker) {
          // Only one thread can be here at a time.
        }
      }
    }

**NB**. les instances de `Type` ne doivent pas être utilisées pour cela (dans le code ci-dessus `typeof(ThreadSafe)`) car les instances de `Type` sont partagées entre AppDomains et donc l'étendue du verrou peut normalement inclure du code qu'il ne devrait pas (par exemple . si `ThreadSafe` est chargé dans deux AppDomains dans le même processus, le verrouillage sur son instance `Type` se verrouillerait mutuellement).

