---
title: "Fundição"
slug: "fundicao"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

*Transmitir* não é o mesmo que *Converter*. É possível converter o valor da string `"-1"` para um valor inteiro (`-1`), mas isso deve ser feito através de métodos de biblioteca como `Convert.ToInt32()` ou `Int32.Parse()`. Isso não pode ser feito usando a sintaxe de conversão diretamente.

## Converta um objeto em um tipo base


## Verificando a compatibilidade sem transmitir
Se você precisa saber se o tipo de um valor estende ou implementa um determinado tipo, mas não quer realmente convertê-lo como esse tipo, você pode usar o operador `is`.

    if(value is int)
    {
       Console.WriteLine(value + "is an int");
    }

## Casting explícito
Se você souber que um valor é de um tipo específico, poderá convertê-lo explicitamente para esse tipo para usá-lo em um contexto em que esse tipo seja necessário.

    object value = -1;
    int number = (int) value;
    Console.WriteLine(Math.Abs(number));

Se tentássemos passar `value` diretamente para `Math.Abs()`, obteríamos uma exceção de tempo de compilação porque `Math.Abs()` não tem uma sobrecarga que recebe um `object` como parâmetro.

Se `value` não pudesse ser convertido em um `int`, a segunda linha neste exemplo lançaria um `InvalidCastException`

## Safe Explicit Casting (operador `as`)
Se você não tiver certeza se um valor é do tipo que você pensa, você pode convertê-lo com segurança usando o operador `as`. Se o valor não for desse tipo, o valor resultante será `null`.

    object value = "-1";
    int? number = value as int?;
    if(number != null)
    {
        Console.WriteLine(Math.Abs(number.Value));
    }

Observe que os valores `null` não têm tipo, então a palavra-chave `as` produzirá com segurança `null` ao converter qualquer valor `null`.

## Fundição Implícita
Um valor será convertido automaticamente para o tipo apropriado se o compilador souber que sempre pode ser convertido para esse tipo.

    int number = -1;
    object value = number;
    Console.WriteLine(value);

Neste exemplo, não precisamos usar a sintaxe típica de conversão explícita porque o compilador sabe que todos os `int`s podem ser convertidos em `object`s. Na verdade, poderíamos evitar criar variáveis ​​e passar `-1` diretamente como o argumento de `Console.WriteLine()` que espera um `objeto`.

    Console.WriteLine(-1);

## Conversões numéricas explícitas
Os operadores de conversão explícitos podem ser usados ​​para realizar conversões de tipos numéricos, mesmo que eles não se estendam ou implementem um ao outro.

    double value = -1.1;
    int number = (int) value;

Observe que nos casos em que o tipo de destino tiver menos precisão que o tipo original, a precisão será perdida. Por exemplo, `-1.1` como um valor duplo no exemplo acima se torna `-1` como um valor inteiro.

Além disso, as conversões numéricas dependem de tipos de tempo de compilação, portanto, não funcionarão se os tipos numéricos tiverem sido "encaixotados" em objetos.

    object value = -1.1;
    int number = (int) value; // throws InvalidCastException


## Operadores de conversão
Em C#, os tipos podem definir *Operadores de conversão* personalizados, que permitem que os valores sejam convertidos de e para outros tipos usando conversões explícitas ou implícitas. Por exemplo, considere uma classe destinada a representar uma expressão JavaScript:

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

Se quiséssemos criar um JsExpression representando uma comparação de dois valores JavaScript, poderíamos fazer algo assim:

    JsExpression intExpression = new JsExpression("-1");
    JsExpression doubleExpression = new JsExpression("-1.0");
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Mas podemos adicionar alguns *operadores de conversão explícitos* ao `JsExpression`, para permitir uma conversão simples ao usar a conversão explícita.

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

Ou podemos alterar esses operadores para *implícitos* para tornar a sintaxe muito mais simples.

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



## Operações de fundição LINQ
Suponha que você tenha tipos como o seguinte:

    interface IThing {  }
    class Thing : IThing {  }

O LINQ permite que você crie uma projeção que altera o tipo genérico de tempo de compilação de um `IEnumerable<>` por meio dos métodos de extensão `Enumerable.Cast<>()` e `Enumerable.OfType<>()`.

    IEnumerable<IThing> things = new IThing[] {new Thing()};
    IEnumerable<Thing> things2 = things.Cast<Thing>();
    IEnumerable<Thing> things3 = things.OfType<Thing>();

Quando `things2` é avaliado, o método `Cast<>()` tentará converter todos os valores em `things` em `Thing`s. Se encontrar um valor que não pode ser convertido, um `InvalidCastException` será lançado.

Quando `things3` é avaliado, o método `OfType<>()` fará o mesmo, exceto que se encontrar um valor que não pode ser convertido, ele simplesmente omitirá esse valor em vez de lançar uma exceção.

Devido ao tipo genérico desses métodos, eles não podem invocar operadores de conversão ou realizar conversões numéricas.

    double[] doubles = new[]{1,2,3}.Cast<double>().ToArray(); // Throws InvalidCastException

Você pode simplesmente executar uma conversão dentro de um `.Select()` como uma solução alternativa:

    double[] doubles = new[]{1,2,3}.Select(i => (double)i).ToArray();

