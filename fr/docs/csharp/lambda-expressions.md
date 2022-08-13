---
title: "Expressions lambda"
slug: "expressions-lambda"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Fermetures
---

Les expressions lambda vont implicitement [capturer les variables utilisées et créer une fermeture][0]. Une fermeture est une fonction associée à un contexte d'état. Le compilateur génère une fermeture chaque fois qu'une expression lambda "enferme" une valeur de son contexte environnant.

Par exemple. lorsque ce qui suit est exécuté

    Func<object, bool> safeApplyFiltererPredicate = o => (o != null) && filterer.Predicate(i);

`safeApplyFilterPredicate` fait référence à un objet nouvellement créé qui a une référence privée à la valeur actuelle de `filterer`, et dont la méthode `Invoke` se comporte comme

    o => (o != null) && filterer.Predicate(i);

Cela peut être important, car tant que la référence à la valeur maintenant dans `safeApplyFilterPredicate` est maintenue, il y aura une référence à l'objet auquel `filterer` se réfère actuellement. Cela a un effet sur le ramasse-miettes et peut provoquer un comportement inattendu si l'objet auquel `filterer` se réfère actuellement est muté.

D'autre part, les fermetures peuvent être utilisées pour délibérément encapsuler un comportement qui implique des références à d'autres objets.

Par exemple.

    var logger = new Logger();
    Func<int, int> Add1AndLog = i => {
        logger.Log("adding 1 to " + i);
        return (i + 1);
    };

Les fermetures peuvent également être utilisées pour modéliser des machines à états :

    Func<int, int> MyAddingMachine() {
        var i = 0;
        return x => i += x;
    };

[0] : http://csharpindepth.com/Articles/Chapter5/Closures.aspx

## Utilisation de la syntaxe lambda pour créer une fermeture
Voir les remarques pour une discussion sur les fermetures. Supposons que nous ayons une interface :

    public interface IMachine<TState, TInput>
    {
        TState State { get; }
        public void Input(TInput input);
    }

puis ce qui suit est exécuté :

    IMachine<int, int> machine = ...;
    Func<int, int> machineClosure = i => {
        machine.Input(i);
        return machine.State;
    };

Maintenant, `machineClosure` fait référence à une fonction de `int` à `int`, qui utilise en arrière-plan l'instance `IMachine` à laquelle `machine` fait référence pour effectuer le calcul. Même si la référence `machine` sort de la portée, tant que l'objet `machineClosure` est maintenu, l'instance originale `IMachine` sera conservée dans le cadre d'une 'fermeture', définie automatiquement par le compilateur.

Attention : cela peut signifier que le même appel de fonction renvoie des valeurs différentes à des moments différents (par exemple, dans cet exemple, si la machine conserve une somme de ses entrées). Dans de nombreux cas, cela peut être inattendu et doit être évité pour tout code de style fonctionnel - les fermetures accidentelles et inattendues peuvent être une source de bogues.

## Expressions lambda de base
    Func<int, int> add1 = i => i + 1;

    Func<int, int, int> add = (i, j) => i + j;

    // Behaviourally equivalent to:

    int Add1(int i)
    {
        return i + 1;
    }

    int Add(int i, int j)
    {
        return i + j;
    }

    ...

    Console.WriteLine(add1(42)); //43
    Console.WriteLine(Add1(42)); //43
    Console.WriteLine(add(100, 250)); //350
    Console.WriteLine(Add(100, 250)); //350

## Expressions lambda de base avec LINQ
    // assume source is {0, 1, 2, ..., 10}

    var evens = source.Where(n => n%2 == 0);
    // evens = {0, 2, 4, ... 10}

    var strings = source.Select(n => n.ToString());
    // strings = {"0", "1", ..., "10"}

## Syntaxe Lambda avec corps de bloc d'instruction
    Func<int, string> doubleThenAddElevenThenQuote = i => {
        var doubled = 2 * i;
        var addedEleven = 11 + doubled;
        return $"'{addedEleven}'";
    };

## Expressions lambda avec System.Linq.Expressions
    Expression<Func<int, bool>> checkEvenExpression = i => i%2 == 0;
    // lambda expression is automatically converted to an Expression<Func<int, bool>>

