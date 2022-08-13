---
title: "İfade Ağaçları"
slug: "ifade-agaclar"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

İfade ağaçları, .NET Framework'te kod ifadelerini temsil etmek için kullanılan veri yapılarıdır. Kod tarafından oluşturulabilir ve kodu başka bir dile çevirmek veya yürütmek için programlı olarak çaprazlanabilirler. İfade Ağaçlarının en popüler oluşturucusu, C# derleyicisinin kendisidir. C# derleyicisi, Expression<Func<...>> türünde bir değişkene bir lambda ifadesi atanırsa, ifade ağaçları oluşturabilir. Genellikle bu, LINQ bağlamında olur. En popüler tüketici, Entity Framework'ün LINQ sağlayıcısıdır. Entity Framework'e verilen ifade ağaçlarını tüketir ve daha sonra veritabanına karşı yürütülen eşdeğer SQL kodu üretir.

## C# Derleyicisi Tarafından Oluşturulan Basit İfade Ağacı
Aşağıdaki C# kodunu göz önünde bulundurun

    Expression<Func<int, int>> expression = a => a + 1;

C# derleyicisi lambda ifadesinin bir temsilci türü yerine bir Expression<T> türüne atandığını gördüğünden, kabaca bu koda eşdeğer bir ifade ağacı oluşturur.

    ParameterExpression parameterA = Expression.Parameter(typeof(int), "a");
    var expression = (Expression<Func<int, int>>)Expression.Lambda(
                                                     Expression.Add(
                                                         parameterA,
                                                         Expression.Constant(1)),
                                                     parameterA);


Ağacın kökü, bir gövde ve bir parametre listesi içeren lambda ifadesidir. Lambda'nın "a" adında 1 parametresi vardır. Gövde, CLR tipi BinaryExpression ve NodeType of Add'in tek bir ifadesidir. Bu ifade toplamayı temsil eder. Sol ve Sağ olarak belirtilen iki alt ifadesi vardır. Sol, "a" parametresi için ParameterExpression'dır ve Sağ, 1 değerine sahip bir ConstantExpression'dır.

Bu ifadenin en basit kullanımı onu yazdırmaktır:

    Console.WriteLine(expression); //prints a => (a + 1)

Hangi eşdeğer C# kodunu yazdırır.

İfade ağacı bir C# temsilcisinde derlenebilir ve CLR tarafından yürütülebilir.

    Func<int, int> lambda = expression.Compile();
    Console.WriteLine(lambda(2)); //prints 3
Genellikle ifadeler SQL gibi diğer dillere çevrilir, ancak yansımaya alternatif olarak genel veya genel olmayan türlerin özel, korumalı ve dahili üyelerini çağırmak için de kullanılabilir.

## form alanı yüklemi oluşturma == değer
Çalışma zamanında `_ => _.Field == "VALUE"` gibi bir ifade oluşturmak için.

Bir "_ => _.Field" yüklemi ve ""DEĞER"` dize değeri verildiğinde, yüklemin doğru olup olmadığını test eden bir ifade oluşturun.

İfade aşağıdakiler için uygundur:

- Yüklemi test etmek için `IQueryable<T>`, `IEnumerable<T>`.
- yüklemi test eden bir 'Where' yan tümcesi oluşturmak için varlık çerçevesi veya 'Linq'ten 'SQL'e.

Bu yöntem, "Alan"ın "DEĞER"e eşit olup olmadığını test eden uygun bir "Eşit" ifadesi oluşturacaktır.

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

Yüklem, bir 'Where' uzantısı yöntemine yüklem dahil edilerek kullanılabilir.

    var predicate = PredicateExtensions.BuildEqualPredicate<Entity>(
        _ => _.Field,
        "VALUE");
    var results = context.Entity.Where(predicate).ToList();



## Statik bir alan almak için ifade
Bunun gibi örnek türüne sahip olmak:

    public TestClass
    {
        public static string StaticPublicField = "StaticPublicFieldValue";
    }

StaticPublicField değerini alabiliriz:

    var fieldExpr = Expression.Field(null, typeof(TestClass), "StaticPublicField");
    var labmda = Expression.Lambda<Func<string>>(fieldExpr);

Daha sonra, yani alan değerini almak için bir temsilci olarak derlenebilir.

    Func<string> retriever = lambda.Compile();
    var fieldValue = retriever();
//fieldValue sonucu StaticPublicFieldValue


## Çağrı İfadesi Sınıfı
[InvocationExpression class][1], aynı Expression ağacının parçaları olan diğer lambda ifadelerinin çağrılmasına izin verir.

Bunları statik 'Expression.Invoke' yöntemiyle yaratırsınız.

Sorun
Açıklamasında "araba" olan öğelere geçmek istiyoruz. İçeride bir dizge aramadan önce null olup olmadığını kontrol etmemiz gerekiyor, ancak hesaplama pahalı olabileceğinden aşırı çağrılmasını istemiyoruz.

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

Çıktı

    element => Invoke(extracted => (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car")), Invoke(e => e.Description, element))
    car
    cargo
    Madagascar
    Predicate is called 5 times.

Unutulmaması gereken ilk şey, bir Invoke içine sarılmış gerçek özellik erişiminin nasıl olduğudur:
    
    Invoke(e => e.Description, element)
'e.Description'a dokunan tek kısım budur ve onun yerine, 'string' türündeki 'extracted' parametresi bir sonrakine iletilir:

    (Not(IsNullOrEmpty(extracted)) AndAlso extracted.Contains("car"))

Burada dikkat edilmesi gereken bir diğer önemli nokta da 'AndAlso'dur. İlk kısım 'yanlış' döndürüyorsa, yalnızca sol kısmı hesaplar. Bunun yerine, her zaman her iki parçayı da hesaplayan ve bu örnekte NullReferenceException ile başarısız olan 'And' bitsel operatörünü kullanmak yaygın bir hatadır.

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.invocationexpression(v=vs.110).aspx

