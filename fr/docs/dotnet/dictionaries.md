---
title: "Dictionnaires"
slug: "dictionnaires"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

## Initialisation d'un dictionnaire avec un initialiseur de collection
    // Translates to `dict.Add(1, "First")` etc.
    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

    // Translates to `dict[1] = "First"` etc.
    // Works in C# 6.0.
    var dict = new Dictionary<int, string>()
    {
        [1] = "First",
        [2] = "Second",
        [3] = "Third"
    };


## Ajout à un dictionnaire
    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict.Add(1, "First");
    dict.Add(2, "Second");
   
    // To safely add items (check to ensure item does not already exist - would throw)
    if(!dict.ContainsKey(3))
    {
       dict.Add(3, "Third");
    }

Alternativement, ils peuvent être ajoutés/définis via un indexeur. (Un indexeur ressemble en interne à une propriété, ayant un get et un set, mais prend un paramètre de n'importe quel type spécifié entre crochets) :

    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict[1] = "First";
    dict[2] = "Second";
    dict[3] = "Third";

Contrairement à la méthode `Add` qui lève une exception, si une clé est déjà contenue dans le dictionnaire, l'indexeur remplace simplement la valeur existante.

Pour un dictionnaire thread-safe, utilisez `ConcurrentDictionary<TKey, TValue>` :

    var dict = new ConcurrentDictionary<int, string>();
    dict.AddOrUpdate(1, "First", (oldKey, oldValue) => "First");



## Obtenir une valeur d'un dictionnaire
Étant donné ce code de configuration :

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Vous voudrez peut-être lire la valeur de l'entrée avec la clé 1. Si la clé n'existe pas, l'obtention d'une valeur lèvera `KeyNotFoundException`, donc vous voudrez peut-être d'abord vérifier cela avec `ContainsKey` :

    if (dict.ContainsKey(1))
        Console.WriteLine(dict[1]);

Cela a un inconvénient : vous effectuerez deux recherches dans votre dictionnaire (une fois pour vérifier l'existence et une pour lire la valeur). Pour un grand dictionnaire, cela peut avoir un impact sur les performances. Heureusement, les deux opérations peuvent être effectuées ensemble :

    string value;
    if (dict.TryGetValue(1, out value))
        Console.WriteLine(value);


## Créez un dictionnaire<chaîne, T> avec des clés insensibles à la casse.
    var MyDict = new Dictionary<string,T>(StringComparison.InvariantCultureIgnoreCase)

## Énumération d'un dictionnaire
Vous pouvez énumérer dans un Dictionnaire de l'une des 3 manières suivantes :

**Utilisation de paires KeyValue**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(KeyValuePair<int, string> kvp in dict) 
    {
       Console.WriteLine("Key : " + kvp.Key.ToString() + ", Value : " + kvp.Value);
    }

**Utilisation des clés**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(int key in dict.Keys)
    {
        Console.WriteLine("Key : " + key.ToString() + ", Value : " + dict[key]);
    }

**Utiliser des valeurs**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(string s in dict.Values)
    {
        Console.WriteLine("Value : " + s);
    }

## ConcurrentDictionary<TKey, TValue> (à partir de .NET 4.0)
> Représente une collection thread-safe de paires clé/valeur qui peuvent être
> accessible par plusieurs threads simultanément.

Créer une instance
--------------------

La création d'une instance fonctionne à peu près de la même manière qu'avec ```Dictionary<TKey, TValue>```, par exemple :

    var dict = new ConcurrentDictionary<int, string>();

Ajout ou mise à jour
------------------

Vous pourriez être surpris qu'il n'y ait pas de méthode `Add`, mais plutôt `AddOrUpdate` avec 2 surcharges :

(1) `AddOrUpdate(TKey key, TValue, Func<TKey, TValue, TValue> addValue)` - *Ajoute une paire clé/valeur si la clé n'existe pas déjà, ou met à jour une paire clé/valeur en utilisant la fonction spécifiée si la clé existe déjà.*

(2) `AddOrUpdate(TKey key, Func<TKey, TValue> addValue, Func<TKey, TValue, TValue> updateValueFactory)` - * Utilise les fonctions spécifiées pour ajouter une paire clé/valeur au si la clé n'existe pas déjà , ou pour mettre à jour une paire clé/valeur si la clé existe déjà.*

Ajout ou mise à jour d'une valeur, quelle qu'en soit la valeur si elle était déjà présente pour une clé donnée (1) :

    string addedValue = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => "First");

Ajout ou mise à jour d'une valeur, mais modification de la valeur dans update, en fonction de la valeur précédente (1) :

    string addedValue2 = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => $"{valueOld} Updated");

En utilisant la surcharge (2), nous pouvons également ajouter une nouvelle valeur en utilisant une usine :

    string addedValue3 = dict.AddOrUpdate(1, (key) => key == 1 ? "First" : "Not First", (updateKey, valueOld) => $"{valueOld} Updated");

Obtenir de la valeur
-----------------
L'obtention d'une valeur est la même qu'avec le `Dictionary<TKey,TValue>` :

    string value = null;
    bool success = dict.TryGetValue(1, out value);

Obtenir ou ajouter une valeur
-------------------------
Il existe deux surcharges de méthode, qui ** obtiendront ou ajouteront ** une valeur de manière thread-safe.

Obtenez la valeur avec la clé 2, ou ajoutez la valeur "Second" si la clé n'est pas présente :

    string theValue = dict.GetOrAdd(2, "Second");

Utilisation d'une fabrique pour ajouter une valeur, si la valeur n'est pas présente :

    string theValue2 = dict.GetOrAdd(2, (key) => key == 2 ? "Second" : "Not Second." );




## IEnumerable vers Dictionnaire (≥ .NET 3.5)
Créez un [Dictionary&lt;TKey, TValue&gt;][1] à partir d'un [IEnumerable&lt;T&gt;][2] :

    using System;
    using System.Collections.Generic;
    using System.Linq;

<b></b>

    public class Fruits
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

<b></b>

    var fruits = new[]
    { 
        new Fruits { Id = 8 , Name = "Apple" },
        new Fruits { Id = 3 , Name = "Banana" },
        new Fruits { Id = 7 , Name = "Mango" },
    };

    
    // Dictionary<int, string>                  key      value
    var dictionary = fruits.ToDictionary(x => x.Id, x => x.Name);

[1] : https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.100).aspx
[2] : https://msdn.microsoft.com/en-us/library/9eekhta0(v=vs.100).aspx



## Suppression d'un dictionnaire
Étant donné ce code de configuration :

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Utilisez la méthode `Remove` pour supprimer une clé et sa valeur associée.

    bool wasRemoved = dict.Remove(2);

L'exécution de ce code supprime la clé "2" et sa valeur du dictionnaire. `Remove` renvoie une valeur booléenne indiquant si la clé spécifiée a été trouvée et supprimée du dictionnaire. Si la clé n'existe pas dans le dictionnaire, rien n'est supprimé du dictionnaire et false est renvoyé (aucune exception n'est levée).

Il est **incorrect** d'essayer de supprimer une clé en définissant la valeur de la clé sur "null".

    dict[2] = null; // WRONG WAY TO REMOVE!

Cela ne supprimera pas la clé. Il remplacera simplement la valeur précédente par une valeur de `null`.

Pour supprimer toutes les clés et valeurs d'un dictionnaire, utilisez la méthode "Clear".

    dict.Clear();

Après avoir exécuté `Clear`, le `Count` du dictionnaire sera 0, mais la capacité interne reste inchangée.

## ContientClé(TKey)
Pour vérifier si un `Dictionnaire` a une clé spécifique, vous pouvez appeler la méthode [`ContainsKey(TKey)`][1] et fournir la clé de type `TKey`. La méthode renvoie une valeur `bool` lorsque la clé existe dans le dictionnaire. Comme échantillon:

    var dictionary = new Dictionary<string, Customer>()
    {
       {"F1", new Customer() { FirstName = "Felipe", ... } },
       {"C2", new Customer() { FirstName = "Carl", ... } },
       {"J7", new Customer() { FirstName = "John", ... } },
       {"M5", new Customer() { FirstName = "Mary", ... } },
    };

Et vérifiez si un `C2` existe sur le dictionnaire :

    if (dictionary.ContainsKey("C2")) 
    {
       // exists
    }

La méthode ContientKey est disponible sur la version générique [`Dictionary<TKey, TValue>`][1].


[1] : https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx

## Dictionnaire vers liste
Création d'une liste de KeyValuePair :

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<KeyValuePair<int, int>> list = new List<KeyValuePair<int, int>>();
    list.AddRange(dictionary);

Création d'une liste de clés :

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Keys);

Création d'une liste de valeurs :

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Values);


## ConcurrentDictionary augmenté avec Lazy'1 réduit le calcul en double
## Problème

ConcurrentDictionary brille lorsqu'il s'agit de renvoyer instantanément des clés existantes à partir du cache, la plupart du temps sans verrouillage, et en concurrence à un niveau granulaire.
Mais que se passe-t-il si la création d'objets est vraiment coûteuse, dépassant le coût du changement de contexte, et que des échecs de cache se produisent ?

Si la même clé est demandée à plusieurs threads, l'un des objets résultant d'opérations en collision sera finalement ajouté à la collection, et les autres seront jetés, gaspillant la ressource CPU pour créer l'objet et la ressource mémoire pour stocker l'objet temporairement . D'autres ressources pourraient également être gaspillées. C'est vraiment mauvais.

## La solution

Nous pouvons combiner `ConcurrentDictionary<TKey, TValue>` avec `Lazy<TValue>`. L'idée est que la méthode ConcurrentDictionary GetOrAdd ne peut renvoyer que la valeur qui a été réellement ajoutée à la collection. Les objets Lazy perdus pourraient également être gaspillés dans ce cas, mais ce n'est pas vraiment un problème, car l'objet Lazy lui-même est relativement peu coûteux. La propriété Value du Lazy perdant n'est jamais demandée, car nous sommes intelligents pour ne demander que la propriété Value de celle réellement ajoutée à la collection - celle renvoyée par la méthode GetOrAdd :

    public static class ConcurrentDictionaryExtensions
    {
        public static TValue GetOrCreateLazy<TKey, TValue>(
            this ConcurrentDictionary<TKey, Lazy<TValue>> d,
            TKey key,
            Func<TKey, TValue> factory)
        {
            return
                d.GetOrAdd(
                    key,
                    key1 =>
                        new Lazy<TValue>(() => factory(key1),
                        LazyThreadSafetyMode.ExecutionAndPublication)).Value;
        }
    }

La mise en cache des objets XmlSerializer peut être particulièrement coûteuse, et il y a aussi beaucoup de conflits au démarrage de l'application. Et il y a plus à cela : s'il s'agit de sérialiseurs personnalisés, il y aura également une fuite de mémoire pour le reste du cycle de vie du processus. Le seul avantage du ConcurrentDictionary dans ce cas est que pour le reste du cycle de vie du processus, il n'y aura pas de verrous, mais le démarrage de l'application et l'utilisation de la mémoire seraient inacceptables. Ceci est un travail pour notre ConcurrentDictionary, augmenté de Lazy :

    private ConcurrentDictionary<Type, Lazy<XmlSerializer>> _serializers =
        new ConcurrentDictionary<Type, Lazy<XmlSerializer>>();
    
    public XmlSerializer GetSerialier(Type t)
    {
        return _serializers.GetOrCreateLazy(t, BuildSerializer);
    }
    
    private XmlSerializer BuildSerializer(Type t)
    {
        throw new NotImplementedException("and this is a homework");
    }

