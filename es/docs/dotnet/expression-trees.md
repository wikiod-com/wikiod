---
title: "Árboles de expresión"
slug: "arboles-de-expresion"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

Los árboles de expresión son estructuras de datos que se utilizan para representar expresiones de código en .NET Framework. Pueden generarse mediante código y atravesarse mediante programación para traducir el código a otro idioma o ejecutarlo. El generador más popular de árboles de expresión es el propio compilador de C#. El compilador de C# puede generar árboles de expresión si se asigna una expresión lambda a una variable de tipo Expression<Func<...>>. Por lo general, esto sucede en el contexto de LINQ. El consumidor más popular es el proveedor LINQ de Entity Framework. Consume los árboles de expresión proporcionados a Entity Framework y genera un código SQL equivalente que luego se ejecuta en la base de datos.

## Árbol de expresión simple generado por el compilador de C#
Considere el siguiente código C#

    Expression<Func<int, int>> expression = a => a + 1;

Debido a que el compilador de C# ve que la expresión lambda se asigna a un tipo Expression<T> en lugar de a un tipo de delegado, genera un árbol de expresión aproximadamente equivalente a este código

    ParameterExpression parameterA = Expression.Parameter(typeof(int), "a");
    var expression = (Expression<Func<int, int>>)Expression.Lambda(
                                                     Expression.Add(
                                                         parameterA,
                                                         Expression.Constant(1)),
                                                     parameterA);


La raíz del árbol es la expresión lambda que contiene un cuerpo y una lista de parámetros. La lambda tiene 1 parámetro llamado "a". El cuerpo es una sola expresión de tipo CLR BinaryExpression y NodeType de Add. Esta expresión representa la suma. Tiene dos subexpresiones denotadas como Left y Right. Left es la expresión de parámetro para el parámetro "a" y Right es una expresión constante con el valor 1.

El uso más simple de esta expresión es imprimirla:

    Console.WriteLine(expression); //prints a => (a + 1)

Que imprime el código C# equivalente.

El árbol de expresión se puede compilar en un delegado de C# y ejecutarse mediante CLR

    Func<int, int> lambda = expression.Compile();
    Console.WriteLine(lambda(2)); //prints 3
Por lo general, las expresiones se traducen a otros lenguajes como SQL, pero también se pueden usar para invocar miembros privados, protegidos e internos de tipos públicos o no públicos como alternativa a Reflection.

## construyendo un predicado de campo de formulario == valor
Para construir una expresión como `_ => _.Field == "VALOR"` en tiempo de ejecución.

Dado un predicado `_ => _.Field` y un valor de cadena `"VALUE"`, cree una expresión que pruebe si el predicado es verdadero o no.

La expresión es adecuada para:

- `IQueryable<T>`, `IEnumerable<T>` para probar el predicado.
- Entity Framework o `Linq` a `SQL` para crear una cláusula `where` que pruebe el predicado.

Este método construirá una expresión `Equal` apropiada que prueba si `Field` es igual a `"VALUE"`.

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

El predicado se puede usar al incluir el predicado en un método de extensión 'Dónde'.

    var predicate = PredicateExtensions.BuildEqualPredicate<Entity>(
        _ => _.Field,
        "VALUE");
    var results = context.Entity.Where(predicate).ToList();



## Expresión para recuperar un campo estático
Tener un tipo de ejemplo como este:

    public TestClass
    {
        public static string StaticPublicField = "StaticPublicFieldValue";
    }

Podemos recuperar el valor de StaticPublicField:

    var fieldExpr = Expression.Field(null, typeof(TestClass), "StaticPublicField");
    var labmda = Expression.Lambda<Func<string>>(fieldExpr);

Se puede entonces, es decir, compilar en un delegado para recuperar el valor del campo.

    Func<string> retriever = lambda.Compile();
    var fieldValue = retriever();
//el resultado del valor del campo es StaticPublicFieldValue


## Clase de expresión de invocación
[Clase InvocaciónExpresión][1] permite la invocación de otras expresiones lambda que son partes del mismo árbol de expresión.

Los creas con el método `Expression.Invoke` estático.

Problema
Queremos entrar en los artículos que tienen "coche" en su descripción. Necesitamos verificar si es nulo antes de buscar una cadena dentro, pero no queremos que se llame en exceso, ya que el cálculo podría ser costoso.

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

Producción

    element => Invoke(extracted => (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car")), Invoke(e => e.Description, element))
    car
    cargo
    Madagascar
    Predicate is called 5 times.

Lo primero a tener en cuenta es cómo accede la propiedad real, envuelta en una Invocación:
    
    Invoke(e => e.Description, element)
, y esta es la única parte que toca `e.Description`, y en su lugar, el parámetro `extraído` de tipo `cadena` se pasa al siguiente:

    (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car"))

Otra cosa importante a tener en cuenta aquí es `AndAlso`. Calcula solo la parte izquierda, si la primera parte devuelve 'falso'. Es un error común usar el operador bit a bit 'Y' en su lugar, que siempre calcula ambas partes y fallaría con una NullReferenceException en este ejemplo.

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.invocationexpression(v=vs.110).aspx

