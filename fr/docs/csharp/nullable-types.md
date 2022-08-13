---
title: "Types nullables"
slug: "types-nullables"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntaxe
- `Nullable<int> i = 10;`
- int? j = 11 ;
- int? k = nul ;
- DateHeure ? DateOfBirth = DateHeure.Maintenant ;
- décimal? Montant = 1,0 m ;
- bah ? Est disponible = vrai ;
- omble ? Lettre = 'a' ;
- (taper)? Nom de variable

Les types Nullable peuvent représenter toutes les valeurs d'un type sous-jacent ainsi que "null".

La syntaxe **`T?`** est un raccourci pour `Nullable<T>`

Les valeurs nulles sont en fait des objets `System.ValueType`, elles peuvent donc être encadrées et non encadrées. De plus, la valeur `null` d'un objet nullable n'est pas la même que la valeur `null` d'un objet de référence, c'est juste un indicateur.

Lorsqu'un objet nullable boxe, la valeur null est convertie en référence "null", et la valeur non nulle est convertie en type sous-jacent non nullable.

    DateTime? dt = null;
    var o = (object)dt;
    var result = (o == null); // is true

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var dt2 = (DateTime)dt; // correct cause o contains DateTime value

La seconde règle conduit à un code correct mais paradoxal :

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var type = o.GetType(); // is DateTime, not Nullable<DateTime>

En bref :

    DateTime? dt = new DateTime(2015, 12, 11);
    var type = dt.GetType(); // is DateTime, not Nullable<DateTime>

## Initialisation d'un nullable


## Vérifier si un Nullable a une valeur


## Récupère la valeur d'un type nullable
Donné suivant nullable `int`

    int? i = 10;

Si une valeur par défaut est nécessaire, vous pouvez en attribuer une à l'aide de [opérateur de coalescence nulle] (https://www.wikiod.com/fr/docs/c%23/37/null-coalescing-operator#t=201512031945577057448), la méthode "GetValueOrDefault" ou vérifier si nullable int `HasValue` avant l'affectation.

    int j = i ?? 0;
    int j = i.GetValueOrDefault(0);
    int j = i.HasValue ? i.Value : 0;

L'utilisation suivante est toujours *unsafe*. Si `i` est nul au moment de l'exécution, une `System.InvalidOperationException` sera levée. Au moment de la conception, si une valeur n'est pas définie, vous obtiendrez une erreur "Utilisation de la variable locale non affectée 'i'".

    int j = i.Value;

## Obtenir une valeur par défaut à partir d'un nullable
La méthode `.GetValueOrDefault()` renvoie une valeur même si la propriété `.HasValue` est fausse (contrairement à la propriété Value, qui lève une exception).

    class Program
    {
        static void Main()
        {
            int? nullableExample = null;
            int result = nullableExample.GetValueOrDefault();
            Console.WriteLine(result); // will output the default value for int - 0
            int secondResult = nullableExample.GetValueOrDefault(1);
            Console.WriteLine(secondResult) // will output our specified default - 1
            int thirdResult = nullableExample ?? 1;
            Console.WriteLine(secondResult) // same as the GetValueOrDefault but a bit shorter
        }
    }
______________________
Production:

    0
    1

## Vérifier si un paramètre de type générique est un type nullable


## La valeur par défaut des types nullables est null
    public class NullableTypesExample
    {
        static int? _testValue;

        public static void Main()
        {
            if(_testValue == null)
                Console.WriteLine("null");
            else
                Console.WriteLine(_testValue.ToString());
        }
    }

Production:

> nul


## Utilisation efficace de l'argument Nullable<T> sous-jacent
Tout type nullable est un type **générique**. Et tout type nullable est un type **valeur**.

Il existe quelques astuces qui permettent d'**utiliser efficacement** le résultat de la méthode [Nullable.GetUnderlyingType][1] lors de la création de code lié à des fins de [réflexion][2]/génération de code :

    public static class TypesHelper {
        public static bool IsNullable(this Type type) {
            Type underlyingType;
            return IsNullable(type, out underlyingType);
        }
        public static bool IsNullable(this Type type, out Type underlyingType) {
            underlyingType = Nullable.GetUnderlyingType(type);
            return underlyingType != null;
        }
        public static Type GetNullable(Type type) {
            Type underlyingType;
            return IsNullable(type, out underlyingType) ? type : NullableTypesCache.Get(type);
        }
        public static bool IsExactOrNullable(this Type type, Func<Type, bool> predicate) {
            Type underlyingType;
            if(IsNullable(type, out underlyingType))
                return IsExactOrNullable(underlyingType, predicate);
            return predicate(type);
        }
        public static bool IsExactOrNullable<T>(this Type type)
            where T : struct {
            return IsExactOrNullable(type, t => Equals(t, typeof(T)));
        }
    }

L'usage:

    Type type = typeof(int).GetNullable();
    Console.WriteLine(type.ToString());
    
    if(type.IsNullable())
        Console.WriteLine("Type is nullable.");
    Type underlyingType;
    if(type.IsNullable(out underlyingType))
        Console.WriteLine("The underlying type is " + underlyingType.Name + ".");
    if(type.IsExactOrNullable<int>())
        Console.WriteLine("Type is either exact or nullable Int32.");
    if(!type.IsExactOrNullable(t => t.IsEnum))
        Console.WriteLine("Type is neither exact nor nullable enum.");

Production:

    System.Nullable`1[System.Int32]
    Type is nullable.
    The underlying type is Int32.
    Type is either exact or nullable Int32.
    Type is neither exact nor nullable enum.

PS. Le `NullableTypesCache` est défini comme suit :

    static class NullableTypesCache {
        readonly static ConcurrentDictionary<Type, Type> cache = new ConcurrentDictionary<Type, Type>();
        static NullableTypesCache() {
            cache.TryAdd(typeof(byte), typeof(Nullable<byte>));
            cache.TryAdd(typeof(short), typeof(Nullable<short>));
            cache.TryAdd(typeof(int), typeof(Nullable<int>));
            cache.TryAdd(typeof(long), typeof(Nullable<long>));
            cache.TryAdd(typeof(float), typeof(Nullable<float>));
            cache.TryAdd(typeof(double), typeof(Nullable<double>));
            cache.TryAdd(typeof(decimal), typeof(Nullable<decimal>));
            cache.TryAdd(typeof(sbyte), typeof(Nullable<sbyte>));
            cache.TryAdd(typeof(ushort), typeof(Nullable<ushort>));
            cache.TryAdd(typeof(uint), typeof(Nullable<uint>));
            cache.TryAdd(typeof(ulong), typeof(Nullable<ulong>));
            //... 
        }
        readonly static Type NullableBase = typeof(Nullable<>);
        internal static Type Get(Type type) {
            // Try to avoid the expensive MakeGenericType method call
            return cache.GetOrAdd(type, t => NullableBase.MakeGenericType(t)); 
        }
    }


[1] : https://msdn.microsoft.com/en-us/library/system.nullable.getunderlyingtype(v=vs.110).aspx
[2] : https://www.wikiod.com/fr/docs/c%23/28/reflection


