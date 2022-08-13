---
title: "Arbres d'expressions"
slug: "arbres-dexpressions"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

Les arborescences d'expressions sont des structures de données utilisées pour représenter des expressions de code dans le .NET Framework. Ils peuvent être générés par code et traversés par programmation pour traduire le code dans un autre langage ou l'exécuter. Le générateur d'arbres d'expression le plus populaire est le compilateur C# lui-même. Le compilateur C# peut générer des arborescences d'expressions si une expression lambda est affectée à une variable de type Expression<Func<...>>. Cela se produit généralement dans le contexte de LINQ. Le consommateur le plus populaire est le fournisseur LINQ d'Entity Framework. Il consomme les arbres d'expression donnés à Entity Framework et génère un code SQL équivalent qui est ensuite exécuté sur la base de données.

## Arbre d'expression simple généré par le compilateur C#
Considérez le code C # suivant

    Expression<Func<int, int>> expression = a => a + 1;

Étant donné que le compilateur C # voit que l'expression lambda est affectée à un type Expression<T> plutôt qu'à un type délégué, il génère une arborescence d'expression à peu près équivalente à ce code

    ParameterExpression parameterA = Expression.Parameter(typeof(int), "a");
    var expression = (Expression<Func<int, int>>)Expression.Lambda(
                                                     Expression.Add(
                                                         parameterA,
                                                         Expression.Constant(1)),
                                                     parameterA);


La racine de l'arbre est l'expression lambda qui contient un corps et une liste de paramètres. Le lambda a 1 paramètre appelé "a". Le corps est une expression unique de type CLR BinaryExpression et NodeType de Add. Cette expression représente l'addition. Il a deux sous-expressions notées Left et Right. Left est la ParameterExpression pour le paramètre "a" et Right est une ConstantExpression avec la valeur 1.

L'utilisation la plus simple de cette expression est de l'imprimer :

    Console.WriteLine(expression); //prints a => (a + 1)

Qui imprime le code C# équivalent.

L'arborescence d'expressions peut être compilée dans un délégué C# et exécutée par le CLR

    Func<int, int> lambda = expression.Compile();
    Console.WriteLine(lambda(2)); //prints 3
Habituellement, les expressions sont traduites dans d'autres langages comme SQL, mais peuvent également être utilisées pour invoquer des membres privés, protégés et internes de types publics ou non publics comme alternative à Reflection.

## construction d'un prédicat de champ de formulaire == valeur
Pour créer une expression comme `_ => _.Field == "VALUE"` lors de l'exécution.

Étant donné un prédicat `_ => _.Field` et une valeur de chaîne `"VALUE"`, créez une expression qui teste si le prédicat est vrai ou non.

L'expression convient pour :

- `IQueryable<T>`, `IEnumerable<T>` pour tester le prédicat.
- structure d'entité ou `Linq` à `SQL` pour créer une clause `Where` qui teste le prédicat.

Cette méthode construira une expression `Equal` appropriée qui teste si `Field` est égal ou non à `"VALUE"`.

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

Le prédicat peut être utilisé en incluant le prédicat dans une méthode d'extension "Où".

    var predicate = PredicateExtensions.BuildEqualPredicate<Entity>(
        _ => _.Field,
        "VALUE");
    var results = context.Entity.Where(predicate).ToList();



## Expression pour récupérer un champ statique
Avoir un exemple de type comme ceci:

    public TestClass
    {
        public static string StaticPublicField = "StaticPublicFieldValue";
    }

Nous pouvons récupérer la valeur de StaticPublicField :

    var fieldExpr = Expression.Field(null, typeof(TestClass), "StaticPublicField");
    var labmda = Expression.Lambda<Func<string>>(fieldExpr);

Il peut ensuite être compilé dans un délégué pour récupérer la valeur du champ.

    Func<string> retriever = lambda.Compile();
    var fieldValue = retriever();
// le résultat de fieldValue est StaticPublicFieldValue


## Classe InvocationExpression
[Classe InvocationExpression][1] permet l'invocation d'autres expressions lambda qui font partie de la même arborescence Expression.

Vous les créez avec la méthode statique `Expression.Invoke`.

Problème
Nous voulons monter sur les articles qui ont "voiture" dans leur description. Nous devons le vérifier pour null avant de rechercher une chaîne à l'intérieur, mais nous ne voulons pas qu'elle soit appelée de manière excessive, car le calcul pourrait être coûteux.

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

Production

    element => Invoke(extracted => (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car")), Invoke(e => e.Description, element))
    car
    cargo
    Madagascar
    Predicate is called 5 times.

La première chose à noter est la façon dont l'accès réel à la propriété, enveloppé dans un Invoke :
    
    Invoke(e => e.Description, element)
, et c'est la seule partie qui touche `e.Description`, et à sa place, le paramètre `extracted` de type `string` est passé au suivant :

    (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car"))

Une autre chose importante à noter ici est `AndAlso`. Il ne calcule que la partie gauche, si la première partie renvoie 'false'. C'est une erreur courante d'utiliser l'opérateur au niveau du bit "Et" à la place, qui calcule toujours les deux parties et échouerait avec une NullReferenceException dans cet exemple.

[1] : https://msdn.microsoft.com/en-us/library/system.linq.expressions.invocationexpression(v=vs.110).aspx

