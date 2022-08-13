---
title: "Recursos do C# 4.0"
slug: "recursos-do-c-40"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Parâmetros opcionais e argumentos nomeados
Podemos omitir o argumento na chamada se esse argumento for um argumento opcional
Cada argumento opcional tem seu próprio valor padrão
Levará o valor padrão se não fornecermos o valor
Um valor padrão de um argumento opcional deve ser um
1. Expressão constante.
2. Deve ser um tipo de valor como enum ou struct.
3. Deve ser uma expressão no formato default(valueType)

Deve ser definido no final da lista de parâmetros

Parâmetros do método com valores padrão:
 
    public void ExampleMethod(int required, string optValue = "test", int optNum = 42)
    {
        //...
    }

Como dito pelo MSDN, Um argumento nomeado,

Permite passar o argumento para a função associando o nome do parâmetro
Não há necessidade de lembrar a posição dos parâmetros que nem sempre estamos cientes.
Não há necessidade de procurar a ordem dos parâmetros na lista de parâmetros da função chamada.
Podemos especificar o parâmetro para cada argumento pelo seu nome.

Argumentos nomeados:
    
    // required = 3, optValue = "test", optNum = 4
    ExampleMethod(3, optNum: 4);
    // required = 2, optValue = "foo", optNum = 42
    ExampleMethod(2, optValue: "foo");
    // required = 6, optValue = "bar", optNum = 1
    ExampleMethod(optNum: 1, optValue: "bar", required: 6);

**Limitação de usar um argumento nomeado**

A especificação do argumento nomeado deve aparecer após a especificação de todos os argumentos fixos.

Se você usar um argumento nomeado antes de um argumento fixo, receberá um erro de tempo de compilação da seguinte maneira.

[![digite a descrição da imagem aqui][1]][1]

A especificação do argumento nomeado deve aparecer após a especificação de todos os argumentos fixos


[1]: http://i.stack.imgur.com/pzWLh.png

## Variação
As interfaces e os delegados genéricos podem ter seus parâmetros de tipo marcados como [_covariant_](https://www.wikiod.com/pt/docs/c%23/27/generics/7362/covariance#t=201607241842437571339) ou [_contravariant_](http:// stackoverflow.com/documentation/c%23/27/generics/7372/contravariance#t=201607241842437571339) usando as palavras-chave `out` e `in`, respectivamente. Essas declarações são então respeitadas para conversões de tipo, implícitas e explícitas, e tanto em tempo de compilação quanto em tempo de execução.

Por exemplo, a interface existente `IEnumerable<T>` foi redefinida como sendo covariante:

    interface IEnumerable<out T>
    {
        IEnumerator<T> GetEnumerator();
    }

A interface existente IComparer<T> foi redefinida como sendo contravariante:

    public interface IComparer<in T>
    {
        int Compare(T x, T y);
    }

## Pesquisa dinâmica de membros
Um novo pseudo-tipo `dinâmico` é introduzido no sistema de tipos C#. É tratado como `System.Object`, mas, além disso, qualquer acesso de membro (chamada de método, campo, propriedade ou acesso de indexador, ou uma invocação de delegado) ou aplicação de um operador em um valor desse tipo é permitido sem nenhum tipo verificação, e sua resolução é adiada até o tempo de execução. Isso é conhecido como tipagem de pato ou ligação tardia. Por exemplo:
 
    // Returns the value of Length property or field of any object
    int GetLength(dynamic obj)
    {
        return obj.Length;
    }
      
    GetLength("Hello, world");        // a string has a Length property,
    GetLength(new int[] { 1, 2, 3 }); // and so does an array,
    GetLength(42);                    // but not an integer - an exception will be thrown
                                      // in GetLength method at run-time

Nesse caso, o tipo dinâmico é usado para evitar uma reflexão mais detalhada. Ele ainda usa o Reflection sob o capô, mas geralmente é mais rápido graças ao cache.

Esse recurso visa principalmente a interoperabilidade com linguagens dinâmicas.

    // Initialize the engine and execute a file
    var runtime = ScriptRuntime.CreateFromConfiguration();
    dynamic globals = runtime.Globals;
    runtime.ExecuteFile("Calc.rb");
    
    // Use Calc type from Ruby
    dynamic calc = globals.Calc.@new();
    calc.valueA = 1337;
    calc.valueB = 666;
    dynamic answer = calc.Calculate();

O tipo dinâmico tem aplicativos mesmo em códigos digitados principalmente estaticamente, por exemplo, torna possível [duplo despacho](https://en.wikipedia.org/wiki/Double_dispatch) sem implementar o padrão Visitor.

## Palavra-chave ref opcional ao usar COM
A palavra-chave ref para chamadores de métodos agora é opcional ao chamar métodos fornecidos por interfaces COM. Dado um método COM com a assinatura

    void Increment(ref int x);
a invocação agora pode ser escrita como

    Increment(0); // no need for "ref" or a place holder variable any more

