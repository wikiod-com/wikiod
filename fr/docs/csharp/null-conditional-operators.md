---
title: "Opérateurs conditionnels nuls"
slug: "operateurs-conditionnels-nuls"
draft: false
images: []
weight: 9614
type: docs
toc: true
---

## Syntaxe
- X?.Y ; //null si X est nul sinon X.Y
- X?.Y?.Z ; //null si X est nul ou Y est nul sinon X.Y.Z
- X?[indice] ; //null si X est nul sinon X[index]
- X?.ValueMethod(); //null si X est nul sinon le résultat de X.ValueMethod();
- X?.VoidMethod(); // ne rien faire si X est nul sinon appeler X.VoidMethod();


Notez que lorsque vous utilisez l'opérateur de coalescence null sur un type de valeur `T`, vous obtiendrez un retour `Nullable<T>`.

## Opérateur conditionnel nul
L'opérateur `?.` est du sucre syntaxique pour éviter les contrôles nuls verbeux. Il est également connu sous le nom de [Safe Navigation Operator] (https://en.wikipedia.org/wiki/Safe_Navigation_Operator).

------

Classe utilisée dans l'exemple suivant :

    public class Person
    {
        public int Age { get; set; }
        public string Name { get; set; }
        public Person Spouse { get; set; }
    }

Si un objet est potentiellement null (comme une fonction qui renvoie un type de référence), l'objet doit d'abord être vérifié pour null pour éviter une éventuelle `NullReferenceException`. Sans l'opérateur conditionnel nul, cela ressemblerait à :

    Person person = GetPerson();

    int? age = null;
    if (person != null)
        age = person.Age;

Le même exemple utilisant l'opérateur null-conditionnel :

    Person person = GetPerson();

    var age = person?.Age;    // 'age' will be of type 'int?', even if 'person' is not null


----------


Chaînage de l'opérateur
---------------------
L'opérateur conditionnel nul peut être combiné sur les membres et sous-membres d'un objet.

    // Will be null if either `person` or `person.Spouse` are null
    int? spouseAge = person?.Spouse?.Age;


----------


Combinaison avec l'opérateur Null-Coalescing
---------------------
L'opérateur conditionnel nul peut être combiné avec l'[opérateur de coalescence nulle][1] pour fournir une valeur par défaut :

    // spouseDisplayName will be "N/A" if person, Spouse, or Name is null
    var spouseDisplayName = person?.Spouse?.Name ?? "N/A";



[1] : https://www.wikiod.com/fr/docs/c%23/37/null-coalescing-operator#t=201610192135170167414

## L'index conditionnel nul
Comme pour l'opérateur `?.`, l'opérateur d'index conditionnel nul vérifie les valeurs nulles lors de l'indexation dans une collection qui peut être nulle.

    string item = collection?[index];

est le sucre syntaxique pour

    string item = null;
    if(collection != null)
    {
        item = collection[index];
    }


## Éviter les exceptions NullReference
    var person = new Person
    {
        Address = null;
    };
    
    var city = person.Address.City; //throws a NullReferenceException
    var nullableCity = person.Address?.City; //returns the value of null

Cet effet peut être enchaîné :

    var person = new Person
    {
        Address = new Address
        {
            State = new State
            {
                Country = null
            }
        }
    };
    
    // this will always return a value of at least "null" to be stored instead
    // of throwing a NullReferenceException
    var countryName = person?.Address?.State?.Country?.Name; 

## L'opérateur conditionnel nul peut être utilisé avec la méthode d'extension
[La méthode d'extension peut fonctionner sur des références nulles] (https://www.wikiod.com/fr/docs/c%23/20/extension-methods/161/null-checking#t=201607271333507907787), mais vous pouvez utiliser `?.` pour null-check de toute façon.

    public class Person 
    {
        public string Name {get; set;}
    }
    
    public static class PersonExtensions
    {
        public static int GetNameLength(this Person person)
        {
            return person == null ? -1 : person.Name.Length;
        }
    }

Normalement, la méthode sera déclenchée pour les références `null` et renverra -1 :

    Person person = null;
    int nameLength = person.GetNameLength(); // returns -1

En utilisant `?.` la méthode ne sera pas déclenchée pour les références `null`, et le [type est `int?`](https://www.wikiod.com/fr/docs/c%23/41/null-conditional-operators/ 173/null-conditional-operator#t=201607271345436018565):

    Person person = null;
    int? nameLength = person?.GetNameLength(); // nameLength is null.

Ce comportement est en fait attendu du fonctionnement de l'opérateur `?.` : il évitera de faire des appels de méthode d'instance pour les instances nulles, afin d'éviter les `NullReferenceExceptions`. Cependant, la même logique s'applique à la méthode d'extension, malgré la différence sur la façon dont la méthode est déclarée.

Pour plus d'informations sur la raison pour laquelle la méthode d'extension est appelée dans le premier exemple, veuillez consulter [Méthodes d'extension - vérification nulle] (https://www.wikiod.com/fr/docs/c%23/20/extension-methods/161/null- vérification#t=201607271333507907787) documentation.

