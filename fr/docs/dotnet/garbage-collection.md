---
title: "Collecte des ordures"
slug: "collecte-des-ordures"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Dans .Net, les objets créés avec new() sont alloués sur le tas géré. Ces objets ne sont jamais explicitement finalisés par le programme qui les utilise ; au lieu de cela, ce processus est contrôlé par le .Net Garbage Collector.

Certains des exemples ci-dessous sont des "cas de laboratoire" pour montrer le Garbage Collector au travail et certains détails significatifs de son comportement, tandis que d'autres se concentrent sur la façon de préparer les classes pour une manipulation correcte par le Garbage Collector.

Le Garbage Collector vise à réduire le coût du programme en termes de mémoire allouée, mais cela a un coût en termes de temps de traitement. Afin d'obtenir un bon compromis global, un certain nombre d'optimisations doivent être prises en compte lors de la programmation avec Garbage Collector à l'esprit :

- Si la méthode Collect() doit être invoquée explicitement (ce qui ne devrait pas souvent être le cas de toute façon), pensez à utiliser le mode "optimisé" qui ne finalise l'objet mort que lorsque la mémoire est réellement nécessaire
- Au lieu d'invoquer la méthode Collect(), envisagez d'utiliser les méthodes AddMemoryPressure() et RemoveMemoryPressure(), qui déclenchent une collecte de mémoire uniquement si nécessaire
- Une collecte de mémoire n'est pas garantie de finaliser tous les objets morts ; à la place, le Garbage Collector gère 3 "générations", un objet "survivant" parfois d'une génération à la suivante
- Plusieurs modèles de threads peuvent s'appliquer, en fonction de divers facteurs, y compris le réglage fin de la configuration, entraînant différents degrés d'interférence entre le thread Garbage Collector et les autres threads d'application.

  

## Un exemple basique de collecte (de déchets)
Soit la classe suivante :

    public class FinalizableObject 
    {
        public FinalizableObject()
        {
            Console.WriteLine("Instance initialized");
        }

        ~FinalizableObject()
        {
            Console.WriteLine("Instance finalized");
        }
    }
Un programme qui crée une instance, même sans l'utiliser :

    new FinalizableObject(); // Object instantiated, ready to be used
Produit la sortie suivante :

    <namespace>.FinalizableObject initialized
Si rien d'autre ne se passe, l'objet n'est pas finalisé jusqu'à la fin du programme (ce qui libère tous les objets sur le tas géré, les finalisant dans le processus).

Il est possible de forcer le Garbage Collector à s'exécuter à un moment donné, comme suit :

    new FinalizableObject(); // Object instantiated, ready to be used
    GC.Collect();
Ce qui produit le résultat suivant :

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
Cette fois, dès que le Garbage Collector a été appelé, l'objet inutilisé (c'est-à-dire "mort") a été finalisé et libéré du tas géré.

## Objets vivants et objets morts - les bases
Règle de base : lorsque la récupération de place se produit, les "objets actifs" sont ceux qui sont encore utilisés, tandis que les "objets morts" sont ceux qui ne sont plus utilisés (toute variable ou champ les référençant, le cas échéant, est sorti de la portée avant que la collecte ne se produise) .

Dans l'exemple suivant (pour plus de commodité, FinalizableObject1 et FinalizableObject2 sont des sous-classes de FinalizableObject de l'exemple ci-dessus et héritent donc du comportement du message d'initialisation/finalisation) :

    var obj1 = new FinalizableObject1(); // Finalizable1 instance allocated here
    var obj2 = new FinalizableObject2(); // Finalizable2 instance allocated here
    obj1 = null; // No more references to the Finalizable1 instance 
    GC.Collect();
La sortie sera :

    <namespace>.FinalizableObject1 initialized
    <namespace>.FinalizableObject2 initialized
    <namespace>.FinalizableObject1 finalized
Au moment où le Garbage Collector est appelé, FinalizableObject1 est un objet mort et est finalisé, tandis que FinalizableObject2 est un objet actif et il est conservé sur le tas géré.

## Plusieurs objets morts
Que se passe-t-il si deux (ou plusieurs) objets autrement morts se référencent ? Ceci est illustré dans l'exemple ci-dessous, en supposant que OtherObject est une propriété publique de FinalizableObject :

    var obj1 = new FinalizableObject1(); 
    var obj2 = new FinalizableObject2();
    obj1.OtherObject = obj2;
    obj2.OtherObject = obj1;
    obj1 = null; // Program no longer references Finalizable1 instance
    obj2 = null; // Program no longer references Finalizable2 instance
    // But the two objects still reference each other
    GC.Collect();
Cela produit la sortie suivante :

    <namespace>.FinalizedObject1 initialized
    <namespace>.FinalizedObject2 initialized
    <namespace>.FinalizedObject1 finalized
    <namespace>.FinalizedObject2 finalized
Les deux objets sont finalisés et libérés du tas géré malgré le référencement l'un de l'autre (car aucune autre référence n'existe à l'un d'eux à partir d'un objet réellement actif).

## Références faibles
Les références faibles sont... des références à d'autres objets (alias "cibles"), mais "faibles" car elles n'empêchent pas ces objets d'être ramassés. En d'autres termes, les références faibles ne comptent pas lorsque le Garbage Collector évalue les objets comme "vivants" ou "morts".

Le code suivant :

    var weak = new WeakReference<FinalizableObject>(new FinalizableObject());
    GC.Collect();
Produit la sortie :

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
L'objet est libéré du tas géré bien qu'il soit référencé par la variable WeakReference (toujours dans la portée lorsque le Garbage collector a été appelé).

Conséquence n°1 : à tout moment, il n'est pas sûr de supposer qu'une cible WeakReference est toujours allouée sur le tas géré ou non.

Conséquence n°2 : chaque fois qu'un programme doit accéder à la cible d'une référence faible, du code doit être fourni dans les deux cas, que la cible soit toujours allouée ou non. La méthode pour accéder à la cible est TryGetTarget :
 
    var target = new object(); // Any object will do as target
    var weak = new WeakReference<object>(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if(weak.TryGetTarget(out target))
    {
        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }

La version générique de WeakReference est disponible depuis .Net 4.5. Toutes les versions du framework fournissent une version non générique et non typée qui est construite de la même manière et vérifiée comme suit :

    var target = new object(); // Any object will do as target
    var weak = new WeakReference(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if (weak.IsAlive)
    {
        target = weak.Target;

        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }


  

## Dispose() vs finaliseurs
Implémentez la méthode Dispose() (et déclarez la classe conteneur comme IDisposable) comme moyen de garantir que toutes les ressources gourmandes en mémoire sont libérées dès que l'objet n'est plus utilisé. Le "hic" est qu'il n'y a aucune garantie forte que la méthode Dispose() sera jamais invoquée (contrairement aux finaliseurs qui sont toujours invoqués à la fin de la vie de l'objet).

Un scénario est un programme appelant Dispose() sur des objets qu'il crée explicitement :

    private void SomeFunction()
    {
        // Initialize an object that uses heavy external resources
        var disposableObject = new ClassThatImplementsIDisposable();

        // ... Use that object

        // Dispose as soon as no longer used
        disposableObject.Dispose();

        // ... Do other stuff 

        // The disposableObject variable gets out of scope here
        // The object will be finalized later on (no guarantee when)
        // But it no longer holds to the heavy external resource after it was disposed
    }


Un autre scénario consiste à déclarer une classe à instancier par le framework. Dans ce cas, la nouvelle classe hérite généralement d'une classe de base, par exemple dans MVC, on crée une classe de contrôleur en tant que sous-classe de System.Web.Mvc.ControllerBase. Lorsque la classe de base implémente l'interface IDisposable, c'est un bon indice que Dispose() serait invoqué correctement par le framework - mais encore une fois, il n'y a aucune garantie solide.

Ainsi Dispose() ne remplace pas un finaliseur ; au lieu de cela, les deux doivent être utilisés à des fins différentes :

- Un finaliseur libère éventuellement des ressources pour éviter les fuites de mémoire qui se produiraient autrement
- Dispose() libère des ressources (éventuellement les mêmes) dès qu'elles ne sont plus nécessaires, pour alléger la pression sur l'allocation globale de mémoire.

## Élimination correcte et finalisation des objets
Comme Dispose() et les finaliseurs ont des objectifs différents, une classe gérant des ressources externes gourmandes en mémoire devrait les implémenter toutes les deux. La conséquence est d'écrire la classe pour qu'elle gère bien deux scénarios possibles :

- Lorsque seul le finaliseur est invoqué
- Lorsque Dispose() est invoqué en premier et ensuite, le finaliseur est également invoqué

Une solution consiste à écrire le code de nettoyage de manière à ce que son exécution une ou deux fois produise le même résultat qu'une seule exécution. La faisabilité dépend de la nature du nettoyage, par exemple :
- La fermeture d'une connexion à une base de données déjà fermée n'aurait probablement aucun effet, donc cela fonctionne
- La mise à jour de certains "comptes d'utilisation" est dangereuse et produirait un mauvais résultat lorsqu'elle est appelée deux fois au lieu d'une.

Une solution plus sûre consiste à s'assurer par conception que le code de nettoyage est appelé une seule fois quel que soit le contexte externe. Ceci peut être réalisé de la "manière classique" en utilisant un indicateur dédié :

    public class DisposableFinalizable1: IDisposable
    {
        private bool disposed = false;

        ~DisposableFinalizable1() { Cleanup(); }

        public void Dispose() { Cleanup(); }

        private void Cleanup()
        {
            if(!disposed)
            {
                // Actual code to release resources gets here, then
                disposed = true;
            }
        }
    }


Alternativement, le Garbage Collector fournit une méthode spécifique SuppressFinalize() qui permet de sauter le finaliseur après l'invocation de Dispose :

    public class DisposableFinalizable2 : IDisposable
    {
        ~DisposableFinalizable2() { Cleanup(); }

        public void Dispose()
        {
            Cleanup();
            GC.SuppressFinalize(this);
        }

        private void Cleanup()
        {
            // Actual code to release resources gets here
        }
    }


 

