---
title: "Árvores de Expressão"
slug: "arvores-de-expressao"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

Árvores de expressão são estruturas de dados usadas para representar expressões de código no .NET Framework. Eles podem ser gerados por código e percorridos programaticamente para traduzir o código para outro idioma ou executá-lo. O gerador mais popular de Árvores de Expressão é o próprio compilador C#. O compilador C# pode gerar árvores de expressão se uma expressão lambda for atribuída a uma variável do tipo Expression<Func<...>>. Geralmente isso acontece no contexto do LINQ. O consumidor mais popular é o provedor LINQ do Entity Framework. Ele consome as árvores de expressão fornecidas ao Entity Framework e gera código SQL equivalente que é então executado no banco de dados.

## Árvore de Expressão Simples Gerada pelo Compilador C#
Considere o seguinte código C#

    Expression<Func<int, int>> expression = a => a + 1;

Como o compilador C# vê que a expressão lambda é atribuída a um tipo Expression<T> em vez de um tipo delegado, ele gera uma árvore de expressão aproximadamente equivalente a esse código

    ParameterExpression parameterA = Expression.Parameter(typeof(int), "a");
    var expression = (Expression<Func<int, int>>)Expression.Lambda(
                                                     Expression.Add(
                                                         parameterA,
                                                         Expression.Constant(1)),
                                                     parameterA);


A raiz da árvore é a expressão lambda que contém um corpo e uma lista de parâmetros. O lambda tem 1 parâmetro chamado "a". O corpo é uma única expressão do tipo CLR BinaryExpression e NodeType de Add. Esta expressão representa adição. Tem duas subexpressões denotadas como Esquerda e Direita. Left é a ParameterExpression para o parâmetro "a" e Right é uma ConstantExpression com o valor 1.

O uso mais simples dessa expressão é imprimi-la:

    Console.WriteLine(expression); //prints a => (a + 1)

Que imprime o código C# equivalente.

A árvore de expressão pode ser compilada em um delegado C# e executada pelo CLR

    Func<int, int> lambda = expression.Compile();
    Console.WriteLine(lambda(2)); //prints 3
Normalmente, as expressões são traduzidas para outras linguagens como SQL, mas também podem ser usadas para invocar membros privados, protegidos e internos de tipos públicos ou não públicos como alternativa ao Reflection.

## construindo um predicado de campo de formulário == valor
Para construir uma expressão como `_ => _.Field == "VALUE"` em tempo de execução.

Dado um predicado `_ => _.Field` e um valor de string `"VALUE"`, crie uma expressão que teste se o predicado é verdadeiro ou não.

A expressão é adequada para:

- `IQueryable<T>`, `IEnumerable<T>` para testar o predicado.
- framework de entidade ou `Linq` para `SQL` para criar uma cláusula `Where` que testa o predicado.

Este método criará uma expressão `Equal` apropriada que testa se `Field` é igual ou não a `"VALUE"`.

    public static Expression<Func<T, bool>> BuildEqualPredicate<T>(
        Expression<Func<T, string>> memberAccessor,
        string term)
    {
        var toString = Expression.Convert(Expression.Constant(term), typeof(string));
        Expression expression = Expression.Equal(memberAccessor.Body, toString);
        var predicate = Expression.Lambda<Func<T, bool>>(
            expression,
            memberAccessor.Parameters);
        return predicate;
    }

O predicado pode ser usado incluindo o predicado em um método de extensão `Where`.

    var predicate = PredicateExtensions.BuildEqualPredicate<Entity>(
        _ => _.Field,
        "VALUE");
    var results = context.Entity.Where(predicate).ToList();



## Expressão para recuperar um campo estático
Tendo o tipo de exemplo assim:

    public TestClass
    {
        public static string StaticPublicField = "StaticPublicFieldValue";
    }

Podemos recuperar o valor de StaticPublicField:

    var fieldExpr = Expression.Field(null, typeof(TestClass), "StaticPublicField");
    var labmda = Expression.Lambda<Func<string>>(fieldExpr);

Ele pode então ser compilado em um delegado para recuperar o valor do campo.

    Func<string> retriever = lambda.Compile();
    var fieldValue = retriever();
// resultado fieldValue é StaticPublicFieldValue


## Classe InvocationExpression
[InvocationExpression class][1] permite a invocação de outras expressões lambda que fazem parte da mesma árvore de Expressão.

Você os cria com o método estático `Expression.Invoke`.

Problema
Queremos entrar nos itens que têm "carro" em sua descrição. Precisamos verificá-lo para nulo antes de procurar uma string dentro, mas não queremos que seja chamado excessivamente, pois a computação pode ser cara.

    using System;
    using System.Linq;
    using System.Linq.Expressions;
                        
    public class Program
    {
        public static void Main()
        {
            var elements = new[] {
                new Element { Description = "car" },
                new Element { Description = "cargo" },
                new Element { Description = "wheel" },
                new Element { Description = null },
                new Element { Description = "Madagascar" },
            };
        
            var elementIsInterestingExpression = CreateSearchPredicate(
                searchTerm: "car",
                whereToSearch: (Element e) => e.Description);
                
            Console.WriteLine(elementIsInterestingExpression.ToString());
                
            var elementIsInteresting = elementIsInterestingExpression.Compile();
            var interestingElements = elements.Where(elementIsInteresting);
            foreach (var e in interestingElements)
            {
                Console.WriteLine(e.Description);
            }
            
            var countExpensiveComputations = 0;
            Action incCount = () => countExpensiveComputations++;
            elements
                .Where(
                    CreateSearchPredicate(
                        "car",
                        (Element e) => ExpensivelyComputed(
                            e, incCount
                        )
                    ).Compile()
                )
                .Count();
                       
            Console.WriteLine("Property extractor is called {0} times.", countExpensiveComputations);
        }
        
        private class Element
        {
            public string Description { get; set; }
        }
        
        private static string ExpensivelyComputed(Element source, Action count)
        {
            count();
            return source.Description;
        }
        
        private static Expression<Func<T, bool>> CreateSearchPredicate<T>(
                string searchTerm,
                Expression<Func<T, string>> whereToSearch)
        {
            var extracted = Expression.Parameter(typeof(string), "extracted");
        
            Expression<Func<string, bool>> coalesceNullCheckWithSearch =
                Expression.Lambda<Func<string, bool>>(
                    Expression.AndAlso(
                        Expression.Not(
                            Expression.Call(typeof(string), "IsNullOrEmpty", null, extracted)
                        ),
                        Expression.Call(extracted, "Contains", null, Expression.Constant(searchTerm))
                    ),
                    extracted);
                    
            var elementParameter = Expression.Parameter(typeof(T), "element");
                    
            return Expression.Lambda<Func<T, bool>>(
                Expression.Invoke(
                    coalesceNullCheckWithSearch,
                    Expression.Invoke(whereToSearch, elementParameter)
                ),
                elementParameter
            );
        }
    }

Resultado

    element => Invoke(extracted => (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car")), Invoke(e => e.Description, element))
    car
    cargo
    Madagascar
    Predicate is called 5 times.

A primeira coisa a notar é como o acesso real da propriedade, envolto em um Invoke:
    
    Invoke(e => e.Description, element)
, e esta é a única parte que toca em `e.Description`, e no lugar dela, o parâmetro `extracted` do tipo `string` é passado para a próxima:

    (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car"))

Outra coisa importante a notar aqui é `AndAlso`. Ele calcula apenas a parte esquerda, se a primeira parte retornar 'false'. É um erro comum usar o operador bit a bit 'And' em vez dele, que sempre calcula ambas as partes e falharia com um NullReferenceException neste exemplo.

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.invocationexpression(v=vs.110).aspx

