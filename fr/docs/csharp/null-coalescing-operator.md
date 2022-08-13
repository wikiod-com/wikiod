---
title: "Opérateur de fusion nulle"
slug: "operateur-de-fusion-nulle"
draft: false
images: []
weight: 9486
type: docs
toc: true
---

## Syntaxe
- var result = possibleNullObject ?? valeur par défaut;

## Paramètres
| Paramètre | Détails |
| --------- | ------- |
| `possibleNullObject` | La valeur à tester pour la valeur nulle. Si non null, cette valeur est retournée. Doit être un type nullable. |
| `valeurpardéfaut` | La valeur renvoyée si `possibleNullObject` est null. Doit être du même type que `possibleNullObject`. |

L'opérateur de fusion nul lui-même est composé de deux caractères de point d'interrogation consécutifs : `??`

C'est un raccourci pour l'expression conditionnelle :

    possibleNullObject != null ? possibleNullObject : defaultValue

L'opérande de gauche (objet testé) doit être un type valeur Nullable ou un type référence, sinon une erreur de compilation se produira.

La ?? L'opérateur fonctionne à la fois pour les types de référence et les types de valeur.



## Utilisation de base
L'utilisation de [`opérateur de coalescence nulle (??)`][2] vous permet de spécifier une valeur par défaut pour un type nullable si l'opérande de gauche est `null`.

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString ?? "not provided"));

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/GNosPU)

Ceci est logiquement équivalent à :

    string testString = null;
    if (testString == null)
    {
        Console.WriteLine("The specified string is - not provided");
    }
    else
    {
        Console.WriteLine("The specified string is - " + testString);
    }

ou en utilisant l'opérateur [opérateur ternaire (?:)][1] :

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString == null ? "not provided" : testString));


[1] : https://www.wikiod.com/fr/docs/c%23/18/operators/6029/ternary-operator#t=201610101110242934481
[2] : https://msdn.microsoft.com/en-us/library/ms173224.aspx

## Chute nulle et chaînage
L'opérande de gauche doit être nullable, tandis que l'opérande de droite peut l'être ou non. Le résultat sera typé en conséquence.

**Non nullable**

    int? a = null;
    int b = 3;
    var output = a ?? b;
    var type = output.GetType();  

    Console.WriteLine($"Output Type :{type}");
    Console.WriteLine($"Output value :{output}");

**Production:**
>Type :Système.Int32
>valeur :3

[Voir la démo][1]

**Annulable**

    int? a = null;
    int? b = null;
    var output = a ?? b;

`output` sera de type `int?` et égal à `b`, ou `null`.

**Coalescence multiple**

La coalescence peut aussi se faire en chaînes :

    int? a = null;
    int? b = null;
    int c = 3;
    var output = a ?? b ?? c;

    var type = output.GetType();    
    Console.WriteLine($"Type :{type}");
    Console.WriteLine($"value :{output}");

**Production:**
>Type :Système.Int32
> valeur :3

[Voir la démo][2]

**Chaînage conditionnel nul**

L'opérateur de coalescence nulle peut être utilisé en tandem avec [l'opérateur de propagation nulle] [3] pour fournir un accès plus sûr aux propriétés des objets.

    object o = null;
    var output = o?.ToString() ?? "Default Value";

**Production:**
>Type :Système.Chaîne
>valeur :Valeur par défaut

[Voir la démo][4]


[1] : https://dotnetfiddle.net/hKHOcN
[2] : https://dotnetfiddle.net/xC8Bmc
[3] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607280322338995462
[4] : https://dotnetfiddle.net/nk1QRn

## Opérateur de coalescence nul avec appels de méthode
L'opérateur de fusion null permet de s'assurer qu'une méthode susceptible de renvoyer "null" reviendra à une valeur par défaut.

Sans l'opérateur de coalescence nul :

    string name = GetName();

    if (name == null)
        name = "Unknown!";

Avec l'opérateur de coalescence nul :

    string name = GetName() ?? "Unknown!";


## Utiliser existant ou créer de nouveaux
Un scénario d'utilisation courant pour lequel cette fonctionnalité est vraiment utile est lorsque vous recherchez un objet dans une collection et que vous devez en créer un nouveau s'il n'existe pas déjà.

    IEnumerable<MyClass> myList = GetMyList();
    var item = myList.SingleOrDefault(x => x.Id == 2) ?? new MyClass { Id = 2 };

## Initialisation des propriétés paresseuses avec un opérateur de coalescence nul
    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars
    {
        get { return _fooBars ?? (_fooBars = new List<FooBar>()); }
    }

La première fois que la propriété `.FooBars` est accédée, la variable `_fooBars` sera évaluée comme `null`, tombant ainsi dans l'instruction d'affectation assigne et évalue à la valeur résultante.

Sécurité des fils
===
Il s'agit d'une manière **non thread-safe** d'implémenter des propriétés paresseuses. Pour la paresse thread-safe, utilisez la classe [`Lazy<T>`][1] intégrée au .NET Framework.

Sucre syntaxique C# 6 utilisant des corps d'expression
====

Notez que depuis C# 6, cette syntaxe peut être simplifiée en utilisant le corps de l'expression pour la propriété :

    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars => _fooBars ?? ( _fooBars = new List<FooBar>() );

Les accès ultérieurs à la propriété donneront la valeur stockée dans la variable `_fooBars`.

Exemple dans le modèle MVVM
===

Ceci est souvent utilisé lors de l'implémentation de commandes dans le modèle MVVM. Au lieu d'initialiser les commandes avidement avec la construction d'un modèle de vue, les commandes sont initialisées paresseusement en utilisant ce modèle comme suit :

    private ICommand _actionCommand = null;
    public ICommand ActionCommand =>
       _actionCommand ?? ( _actionCommand = new DelegateCommand( DoAction ) );


[1] : https://www.wikiod.com/fr/docs/c%23/1192/singleton-implementation/6795/lazy-thread-safe-singleton-using-lazyt

