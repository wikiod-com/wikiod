---
title: "Expressões lambda"
slug: "expressoes-lambda"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Fechamentos
---

As expressões lambda implicitamente [capturarão as variáveis ​​usadas e criarão um encerramento][0]. Um fechamento é uma função junto com algum contexto de estado. O compilador irá gerar um encerramento sempre que uma expressão lambda 'incluir' um valor de seu contexto circundante.

Por exemplo. quando o seguinte é executado

    Func<object, bool> safeApplyFiltererPredicate = o => (o != null) && filterer.Predicate(i);

`safeApplyFilterPredicate` refere-se a um objeto recém-criado que tem uma referência privada ao valor atual de `filterer`, e cujo método `Invoke` se comporta como

    o => (o != null) && filterer.Predicate(i);

Isso pode ser importante, porque enquanto a referência ao valor agora em `safeApplyFilterPredicate` for mantida, haverá uma referência ao objeto ao qual `filterer` se refere atualmente. Isso tem um efeito na coleta de lixo e pode causar um comportamento inesperado se o objeto ao qual o `filterer` se refere atualmente for modificado.

Por outro lado, closures podem ser usados ​​para efeito deliberado para encapsular um comportamento que envolve referências a outros objetos.

Por exemplo.

    var logger = new Logger();
    Func<int, int> Add1AndLog = i => {
        logger.Log("adding 1 to " + i);
        return (i + 1);
    };

Closures também podem ser usados ​​para modelar máquinas de estado:

    Func<int, int> MyAddingMachine() {
        var i = 0;
        return x => i += x;
    };

[0]: http://csharpindepth.com/Articles/Chapter5/Closures.aspx

## Usando a sintaxe lambda para criar um encerramento
Veja as observações para discussão sobre fechamentos. Suponha que temos uma interface:

    public interface IMachine<TState, TInput>
    {
        TState State { get; }
        public void Input(TInput input);
    }

e então o seguinte é executado:

    IMachine<int, int> machine = ...;
    Func<int, int> machineClosure = i => {
        machine.Input(i);
        return machine.State;
    };

Agora `machineClosure` se refere a uma função de `int` para `int`, que nos bastidores usa a instância `IMachine` à qual `machine` se refere para realizar a computação. Mesmo que a referência `machine` saia do escopo, desde que o objeto `machineClosure` seja mantido, a instância original `IMachine` será mantida como parte de um 'closure', definido automaticamente pelo compilador.

Atenção: isso pode significar que a mesma chamada de função retorna valores diferentes em momentos diferentes (por exemplo, neste exemplo, se a máquina mantém uma soma de suas entradas). Em muitos casos, isso pode ser inesperado e deve ser evitado para qualquer código em estilo funcional - fechamentos acidentais e inesperados podem ser uma fonte de bugs.

## Expressões lambda básicas
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

## Expressões lambda básicas com LINQ
    // assume source is {0, 1, 2, ..., 10}

    var evens = source.Where(n => n%2 == 0);
    // evens = {0, 2, 4, ... 10}

    var strings = source.Select(n => n.ToString());
    // strings = {"0", "1", ..., "10"}

## Sintaxe lambda com corpo de bloco de instrução
    Func<int, string> doubleThenAddElevenThenQuote = i => {
        var doubled = 2 * i;
        var addedEleven = 11 + doubled;
        return $"'{addedEleven}'";
    };

## Expressões lambda com System.Linq.Expressions
    Expression<Func<int, bool>> checkEvenExpression = i => i%2 == 0;
    // lambda expression is automatically converted to an Expression<Func<int, bool>>

