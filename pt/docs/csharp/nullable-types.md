---
title: "Tipos anuláveis"
slug: "tipos-anulaveis"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Sintaxe
- `Nulável<int> i = 10;`
- int? j = 11;
- int? k = nulo;
- Data hora? DataDoNascimento = DataHora.Agora;
- decimal? Quantidade = 1,0m;
- bool? EstáDisponível = true;
- Caracteres? Letra = 'a';
- (modelo)? nome variável

Tipos anuláveis ​​podem representar todos os valores de um tipo subjacente, bem como `null`.

A sintaxe **`T?`** é um atalho para `Nullable<T>`

Valores anuláveis ​​são objetos `System.ValueType` na verdade, então eles podem ser boxed e unboxed. Além disso, o valor `null` de um objeto anulável não é o mesmo que o valor `null` de um objeto de referência, é apenas um sinalizador.

Quando um box de objeto anulável, o valor nulo é convertido em referência `null` e o valor não nulo é convertido em tipo subjacente não anulável.

    DateTime? dt = null;
    var o = (object)dt;
    var result = (o == null); // is true

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var dt2 = (DateTime)dt; // correct cause o contains DateTime value

A segunda regra leva a um código correto, mas paradoxal:

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var type = o.GetType(); // is DateTime, not Nullable<DateTime>

Em forma curta:

    DateTime? dt = new DateTime(2015, 12, 11);
    var type = dt.GetType(); // is DateTime, not Nullable<DateTime>

## Inicializando um valor nulo


## Verifica se um Nullable tem um valor


## Obtém o valor de um tipo anulável
Dado o seguinte `int` anulável

    int? i = 10;

Caso o valor padrão seja necessário, você pode atribuir um usando [null coalescing operator](https://www.wikiod.com/pt/docs/c%23/37/null-coalescing-operator#t=201512031945577057448), o método `GetValueOrDefault` ou verifique se nullable int `HasValue` antes da atribuição.

    int j = i ?? 0;
    int j = i.GetValueOrDefault(0);
    int j = i.HasValue ? i.Value : 0;

O uso a seguir é sempre *inseguro*. Se `i` for nulo em tempo de execução, um `System.InvalidOperationException` será lançado. Em tempo de design, se um valor não for definido, você receberá um erro `Use of unassigned local variable 'i'`.

    int j = i.Value;

## Obtendo um valor padrão de um valor nulo
O método `.GetValueOrDefault()` retorna um valor mesmo que a propriedade `.HasValue` seja falsa (diferentemente da propriedade Value, que lança uma exceção).

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
Resultado:

    0
    1

## Verifique se um parâmetro de tipo genérico é um tipo anulável


## O valor padrão dos tipos anuláveis ​​é nulo
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

Resultado:

> nulo


## Uso efetivo do argumento Nullable<T> subjacente
Qualquer tipo anulável é um tipo **genérico**. E qualquer tipo anulável é um tipo **valor**.

Existem alguns truques que permitem **usar efetivamente** o resultado do método [Nullable.GetUnderlyingType][1] ao criar código relacionado a [reflexão][2]/fins de geração de código:

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

O uso:

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

Resultado:

    System.Nullable`1[System.Int32]
    Type is nullable.
    The underlying type is Int32.
    Type is either exact or nullable Int32.
    Type is neither exact nor nullable enum.

PS. O `NullableTypesCache` é definido da seguinte forma:

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


[1]: https://msdn.microsoft.com/en-us/library/system.nullable.getunderlyingtype(v=vs.110).aspx
[2]: https://www.wikiod.com/pt/docs/c%23/28/reflection


