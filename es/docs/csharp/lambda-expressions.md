---
title: "Expresiones Lambda"
slug: "expresiones-lambda"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Cierres
---

Las expresiones lambda implícitamente [capturarán las variables utilizadas y crearán un cierre][0]. Un cierre es una función junto con algún contexto de estado. El compilador generará un cierre siempre que una expresión lambda 'encierre' un valor de su contexto circundante.

P.ej. cuando se ejecuta lo siguiente

    Func<object, bool> safeApplyFiltererPredicate = o => (o != null) && filterer.Predicate(i);

`safeApplyFilterPredicate` se refiere a un objeto recién creado que tiene una referencia privada al valor actual de `filterer`, y cuyo método `Invoke` se comporta como

    o => (o != null) && filterer.Predicate(i);

Esto puede ser importante, porque mientras se mantenga la referencia al valor ahora en `safeApplyFilterPredicate`, habrá una referencia al objeto al que se refiere actualmente `filterer`. Esto tiene un efecto en la recolección de basura y puede causar un comportamiento inesperado si el objeto al que se refiere actualmente el "filtro" está mutado.

Por otro lado, los cierres se pueden usar con efecto deliberado para encapsular un comportamiento que implica referencias a otros objetos.

P.ej.

    var logger = new Logger();
    Func<int, int> Add1AndLog = i => {
        logger.Log("adding 1 to " + i);
        return (i + 1);
    };

Los cierres también se pueden usar para modelar máquinas de estado:

    Func<int, int> MyAddingMachine() {
        var i = 0;
        return x => i += x;
    };

[0]: http://csharpindepth.com/Articles/Chapter5/Closures.aspx

## Usando la sintaxis lambda para crear un cierre
Ver comentarios para la discusión de los cierres. Supongamos que tenemos una interfaz:

    public interface IMachine<TState, TInput>
    {
        TState State { get; }
        public void Input(TInput input);
    }

y luego se ejecuta lo siguiente:

    IMachine<int, int> machine = ...;
    Func<int, int> machineClosure = i => {
        machine.Input(i);
        return machine.State;
    };

Ahora `machineClosure` se refiere a una función de `int` a `int`, que detrás de escena usa la instancia `IMachine` a la que se refiere `machine` para realizar el cálculo. Incluso si la referencia 'máquina' queda fuera del alcance, siempre que se mantenga el objeto 'máquinaClosure', la instancia original de 'IMachine' se conservará como parte de un 'cierre', definido automáticamente por el compilador.

Advertencia: esto puede significar que la misma llamada de función devuelve diferentes valores en diferentes momentos (por ejemplo, en este ejemplo, si la máquina mantiene una suma de sus entradas). En muchos casos, esto puede ser inesperado y debe evitarse para cualquier código en un estilo funcional: los cierres accidentales e inesperados pueden ser una fuente de errores.

## Expresiones lambda básicas
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

## Expresiones lambda básicas con LINQ
    // assume source is {0, 1, 2, ..., 10}

    var evens = source.Where(n => n%2 == 0);
    // evens = {0, 2, 4, ... 10}

    var strings = source.Select(n => n.ToString());
    // strings = {"0", "1", ..., "10"}

## Sintaxis lambda con cuerpo de bloque de instrucciones
    Func<int, string> doubleThenAddElevenThenQuote = i => {
        var doubled = 2 * i;
        var addedEleven = 11 + doubled;
        return $"'{addedEleven}'";
    };

## Expresiones lambda con System.Linq.Expressions
    Expression<Func<int, bool>> checkEvenExpression = i => i%2 == 0;
    // lambda expression is automatically converted to an Expression<Func<int, bool>>

