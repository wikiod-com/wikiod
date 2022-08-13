---
title: "Fonctionnalités C# 4.0"
slug: "fonctionnalites-c-40"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Paramètres optionnels et arguments nommés
Nous pouvons omettre l'argument dans l'appel si cet argument est un argument facultatif
Chaque argument facultatif a sa propre valeur par défaut
Il prendra la valeur par défaut si nous ne fournissons pas la valeur
La valeur par défaut d'un argument facultatif doit être un
1. Expression constante.
2. Doit être un type de valeur tel qu'enum ou struct.
3. Doit être une expression de la forme default(valueType)

Il doit être défini à la fin de la liste des paramètres

Paramètres de méthode avec des valeurs par défaut :
 
    public void ExampleMethod(int required, string optValue = "test", int optNum = 42)
    {
        //...
    }

Comme dit par MSDN, Un argument nommé ,

Permet de passer l'argument à la fonction en associant le nom du paramètre
Pas besoin de se souvenir de la position des paramètres dont nous ne sommes pas toujours conscients.
Inutile de regarder l'ordre des paramètres dans la liste des paramètres de la fonction appelée.
Nous pouvons spécifier le paramètre pour chaque argument par son nom.

Arguments nommés :
    
    // required = 3, optValue = "test", optNum = 4
    ExampleMethod(3, optNum: 4);
    // required = 2, optValue = "foo", optNum = 42
    ExampleMethod(2, optValue: "foo");
    // required = 6, optValue = "bar", optNum = 1
    ExampleMethod(optNum: 1, optValue: "bar", required: 6);

**Limitation de l'utilisation d'un argument nommé**

La spécification de l'argument nommé doit apparaître après que tous les arguments fixes ont été spécifiés.

Si vous utilisez un argument nommé avant un argument fixe, vous obtiendrez une erreur de compilation comme suit.

[![entrez la description de l'image ici][1]][1]

La spécification de l'argument nommé doit apparaître après que tous les arguments fixes ont été spécifiés


[1] : http://i.stack.imgur.com/pzWLh.png

## Écart
Les interfaces génériques et les délégués peuvent avoir leurs paramètres de type marqués comme [_covariant_](https://www.wikiod.com/fr/docs/c%23/27/generics/7362/covariance#t=201607241842437571339) ou [_contravariant_](http:// stackoverflow.com/documentation/c%23/27/generics/7372/contravariance#t=201607241842437571339) en utilisant respectivement les mots clés "out" et "in". Ces déclarations sont ensuite respectées pour les conversions de type, à la fois implicites et explicites, et à la fois à la compilation et à l'exécution.

Par exemple, l'interface existante `IEnumerable<T>` a été redéfinie comme étant covariante :

    interface IEnumerable<out T>
    {
        IEnumerator<T> GetEnumerator();
    }

L'interface existante IComparer<T> a été redéfinie comme étant contravariante :

    public interface IComparer<in T>
    {
        int Compare(T x, T y);
    }

## Recherche de membre dynamique
Un nouveau pseudo-type `dynamic` est introduit dans le système de type C#. Il est traité comme `System.Object`, mais en plus, tout accès membre (appel de méthode, champ, propriété ou accès indexeur, ou invocation déléguée) ou application d'un opérateur sur une valeur de ce type est autorisé sans aucun type vérification, et sa résolution est reportée jusqu'à l'exécution. C'est ce qu'on appelle le duck typing ou la liaison tardive. Par exemple:
 
    // Returns the value of Length property or field of any object
    int GetLength(dynamic obj)
    {
        return obj.Length;
    }
      
    GetLength("Hello, world");        // a string has a Length property,
    GetLength(new int[] { 1, 2, 3 }); // and so does an array,
    GetLength(42);                    // but not an integer - an exception will be thrown
                                      // in GetLength method at run-time

Dans ce cas, le type dynamique est utilisé pour éviter une réflexion plus détaillée. Il utilise toujours Reflection sous le capot, mais il est généralement plus rapide grâce à la mise en cache.

Cette fonctionnalité vise principalement l'interopérabilité avec les langages dynamiques.

    // Initialize the engine and execute a file
    var runtime = ScriptRuntime.CreateFromConfiguration();
    dynamic globals = runtime.Globals;
    runtime.ExecuteFile("Calc.rb");
    
    // Use Calc type from Ruby
    dynamic calc = globals.Calc.@new();
    calc.valueA = 1337;
    calc.valueB = 666;
    dynamic answer = calc.Calculate();

Le type dynamique a des applications même dans le code principalement typé statiquement, par exemple, il rend [double dispatch](https://en.wikipedia.org/wiki/Double_dispatch) possible sans implémenter le modèle Visitor.

## Mot-clé de référence facultatif lors de l'utilisation de COM
Le mot-clé ref pour les appelants de méthodes est désormais facultatif lors de l'appel de méthodes fournies par les interfaces COM. Étant donné une méthode COM avec la signature

    void Increment(ref int x);
l'invocation peut maintenant s'écrire soit

    Increment(0); // no need for "ref" or a place holder variable any more

