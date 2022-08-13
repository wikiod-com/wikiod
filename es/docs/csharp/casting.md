---
title: "Fundición"
slug: "fundicion"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

*Castear* no es lo mismo que *Convertir*. Es posible convertir el valor de cadena `"-1"` a un valor entero (`-1`), pero esto debe hacerse a través de métodos de biblioteca como `Convert.ToInt32()` o `Int32.Parse()`. No se puede hacer usando la sintaxis de conversión directamente.

## Convierte un objeto en un tipo base


## Comprobación de compatibilidad sin conversión
Si necesita saber si el tipo de un valor se extiende o implementa un tipo dado, pero no quiere convertirlo en ese tipo, puede usar el operador `is`.

    if(value is int)
    {
       Console.WriteLine(value + "is an int");
    }

## Casting explícito
Si sabe que un valor es de un tipo específico, puede convertirlo explícitamente en ese tipo para usarlo en un contexto donde se necesita ese tipo.

    object value = -1;
    int number = (int) value;
    Console.WriteLine(Math.Abs(number));

Si intentamos pasar `valor` directamente a `Math.Abs()`, obtendríamos una excepción en tiempo de compilación porque `Math.Abs()` no tiene una sobrecarga que toma un `objeto` como parámetro.

Si `value` no se puede convertir a un `int`, entonces la segunda línea de este ejemplo generaría una `InvalidCastException`

## Casting explícito seguro (operador `as`)
Si no está seguro de si un valor es del tipo que cree que es, puede convertirlo de forma segura utilizando el operador `as`. Si el valor no es de ese tipo, el valor resultante será `null`.

    object value = "-1";
    int? number = value as int?;
    if(number != null)
    {
        Console.WriteLine(Math.Abs(number.Value));
    }

Tenga en cuenta que los valores `null` no tienen tipo, por lo que la palabra clave `as` generará de forma segura `null` al convertir cualquier valor `null`.

## Casting implícito
Un valor se convertirá automáticamente al tipo apropiado si el compilador sabe que siempre se puede convertir a ese tipo.

    int number = -1;
    object value = number;
    Console.WriteLine(value);

En este ejemplo, no necesitábamos usar la típica sintaxis de conversión explícita porque el compilador sabe que todos los 'int' se pueden convertir en 'objetos'. De hecho, podríamos evitar crear variables y pasar `-1` directamente como argumento de `Console.WriteLine()` que espera un `objeto`.

    Console.WriteLine(-1);

## Conversiones numéricas explícitas
Los operadores de conversión explícitos se pueden usar para realizar conversiones de tipos numéricos, aunque no se extiendan ni implementen entre sí.

    double value = -1.1;
    int number = (int) value;

Tenga en cuenta que en los casos en que el tipo de destino tenga menos precisión que el tipo original, se perderá la precisión. Por ejemplo, `-1.1` como valor doble en el ejemplo anterior se convierte en `-1` como valor entero.

Además, las conversiones numéricas se basan en tipos de tiempo de compilación, por lo que no funcionarán si los tipos numéricos se han "encajonado" en objetos.

    object value = -1.1;
    int number = (int) value; // throws InvalidCastException


## Operadores de conversión
En C#, los tipos pueden definir *Operadores de conversión* personalizados, que permiten que los valores se conviertan hacia y desde otros tipos mediante conversiones explícitas o implícitas. Por ejemplo, considere una clase que pretende representar una expresión de JavaScript:

    public class JsExpression
    {
        private readonly string expression;
        public JsExpression(string rawExpression)
        {
            this.expression = rawExpression;
        }
        public override string ToString()
        {
            return this.expression;
        }
        public JsExpression IsEqualTo(JsExpression other)
        {
            return new JsExpression("(" + this + " == " + other + ")");
        }
    }

Si quisiéramos crear una JsExpression que represente una comparación de dos valores de JavaScript, podríamos hacer algo como esto:

    JsExpression intExpression = new JsExpression("-1");
    JsExpression doubleExpression = new JsExpression("-1.0");
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Pero podemos agregar algunos *operadores de conversión explícitos* a `JsExpression`, para permitir una conversión simple cuando se usa el casting explícito.

    public static explicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static explicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = (JsExpression)(-1);
    JsExpression doubleExpression = (JsExpression)(-1.0);
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

O bien, podríamos cambiar estos operadores a *implícito* para que la sintaxis sea mucho más simple.

    public static implicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static implicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = -1;
    Console.WriteLine(intExpression.IsEqualTo(-1.0)); // (-1 == -1.0)



## Operaciones de fundición de LINQ
Supongamos que tiene tipos como los siguientes:

    interface IThing {  }
    class Thing : IThing {  }

LINQ le permite crear una proyección que cambia el tipo genérico en tiempo de compilación de un `IEnumerable<>` a través de los métodos de extensión `Enumerable.Cast<>()` y `Enumerable.OfType<>()`.

    IEnumerable<IThing> things = new IThing[] {new Thing()};
    IEnumerable<Thing> things2 = things.Cast<Thing>();
    IEnumerable<Thing> things3 = things.OfType<Thing>();

Cuando se evalúa `things2`, el método `Cast<>()` intentará convertir todos los valores de `things` en `Thing`s. Si encuentra un valor que no se puede convertir, se lanzará una `InvalidCastException`.

Cuando se evalúa `things3`, el método `OfType<>()` hará lo mismo, excepto que si encuentra un valor que no se puede convertir, simplemente omitirá ese valor en lugar de lanzar una excepción.

Debido al tipo genérico de estos métodos, no pueden invocar operadores de conversión ni realizar conversiones numéricas.

    double[] doubles = new[]{1,2,3}.Cast<double>().ToArray(); // Throws InvalidCastException

Simplemente puede realizar una conversión dentro de `.Select()` como solución alternativa:

    double[] doubles = new[]{1,2,3}.Select(i => (double)i).ToArray();

