---
title: "Tipos anulables"
slug: "tipos-anulables"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Sintaxis
- `Anulable<int> i = 10;`
- ¿En t? j = 11;
- ¿En t? k = nulo;
- ¿Fecha y hora? FechaDeNacimiento = FechaHora.Ahora;
- decimal? Cantidad = 1,0 m;
- bool? Está disponible = verdadero;
- char? Letra = 'a';
- (escribe)? nombre de la variable

Los tipos anulables pueden representar todos los valores de un tipo subyacente, así como `null`.

La sintaxis **`T?`** es una abreviatura de `Nullable<T>`

Los valores anulables son objetos `System.ValueType` en realidad, por lo que se pueden encuadrar y desencuadrar. Además, el valor `nulo` de un objeto anulable no es lo mismo que el valor `nulo` de un objeto de referencia, es solo una bandera.

Cuando se coloca un objeto anulable, el valor nulo se convierte en una referencia "nula", y el valor no nulo se convierte en un tipo subyacente no anulable.

    DateTime? dt = null;
    var o = (object)dt;
    var result = (o == null); // is true

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var dt2 = (DateTime)dt; // correct cause o contains DateTime value

La segunda regla conduce a un código correcto, pero paradójico:

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var type = o.GetType(); // is DateTime, not Nullable<DateTime>

En forma abreviada:

    DateTime? dt = new DateTime(2015, 12, 11);
    var type = dt.GetType(); // is DateTime, not Nullable<DateTime>

## Inicializando un anulable


## Comprobar si un Nullable tiene un valor


## Obtener el valor de un tipo anulable
Dado el siguiente `int` anulable

    int? i = 10;

En caso de que se necesite un valor predeterminado, puede asignar uno usando [operador coalescente nulo] (https://www.wikiod.com/es/docs/c%23/37/null-coalescing-operator#t=201512031945577057448), método `GetValueOrDefault` o comprobar si se puede anular int `HasValue` antes de la asignación.

    int j = i ?? 0;
    int j = i.GetValueOrDefault(0);
    int j = i.HasValue ? i.Value : 0;

El siguiente uso es siempre *inseguro*. Si `i` es nulo en tiempo de ejecución, se lanzará una `System.InvalidOperationException`. En el momento del diseño, si no se establece un valor, obtendrá un error `Uso de variable local no asignada 'i'`.

    int j = i.Value;

## Obtener un valor predeterminado de un anulable
El método `.GetValueOrDefault()` devuelve un valor incluso si la propiedad `.HasValue` es falsa (a diferencia de la propiedad Value, que genera una excepción).

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
Producción:

    0
    1

## Comprobar si un parámetro de tipo genérico es un tipo anulable


## El valor predeterminado de los tipos anulables es nulo
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

Producción:

> nulo


## Uso efectivo del argumento Nullable<T> subyacente
Cualquier tipo anulable es un tipo **genérico**. Y cualquier tipo anulable es un tipo de **valor**.

Hay algunos trucos que permiten **utilizar eficazmente** el resultado del método [Nullable.GetUnderlyingType][1] al crear código relacionado con [reflejo][2]/propósitos de generación de código:

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

El uso:

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

Producción:

    System.Nullable`1[System.Int32]
    Type is nullable.
    The underlying type is Int32.
    Type is either exact or nullable Int32.
    Type is neither exact nor nullable enum.

PD. El `NullableTypesCache` se define de la siguiente manera:

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
[2]: https://www.wikiod.com/es/docs/c%23/28/reflection


