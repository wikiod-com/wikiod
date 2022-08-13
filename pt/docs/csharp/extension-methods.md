---
title: "Métodos de extensão"
slug: "metodos-de-extensao"
draft: false
images: []
weight: 6772
type: docs
toc: true
---

## Sintaxe
- public static ReturnType MyExtensionMethod (este destino TargetType)
- public static ReturnType MyExtensionMethod(este destino TargetType, TArg1 arg1, ...)

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| isso | O primeiro parâmetro de um método de extensão deve sempre ser precedido pela palavra-chave `this`, seguida pelo identificador com o qual se referirá à instância "atual" do objeto que você está estendendo |


Os métodos de extensão são açúcares sintáticos que permitem que métodos estáticos sejam invocados em instâncias de objetos como se fossem membros do próprio tipo.

Os métodos de extensão requerem um objeto de destino explícito. Você precisará usar a palavra-chave `this` para acessar o método de dentro do próprio tipo estendido.

Os métodos de extensão devem ser declarados como estáticos e devem residir em uma classe estática.

**Qual namespace?**

A escolha do namespace para sua classe de método de extensão é uma troca entre visibilidade e descoberta.

A [opção][1] mais comumente mencionada é ter um namespace personalizado para seus métodos de extensão. No entanto, isso envolverá um esforço de comunicação para que os usuários do seu código saibam que os métodos de extensão existem e onde encontrá-los.

Uma alternativa é escolher um namespace para que os desenvolvedores descubram seus métodos de extensão por meio do Intellisense. Portanto, se você deseja estender a classe `Foo`, é lógico colocar os métodos de extensão no mesmo namespace que `Foo`.

É importante perceber que **nada impede que você use o namespace de "alguém"**: Assim, se você quiser estender `IEnumerable`, você pode adicionar seu método de extensão no namespace `System.Linq`.

Isso nem *sempre* é uma boa ideia. Por exemplo, em um caso específico, você pode querer estender um tipo comum (`bool IsApproxEqualTo(this double value, double other)` por exemplo), mas não ter que 'poluir' todo o `System`. Nesse caso, é preferível escolher um namespace local e específico.

Finalmente, também é possível colocar os métodos de extensão em *nenhum namespace*!

Uma boa pergunta de referência: [Como você gerencia os namespaces de seus métodos de extensão?][2]

**Aplicabilidade**

Deve-se tomar cuidado ao criar métodos de extensão para garantir que eles sejam apropriados para todas as entradas possíveis e não sejam relevantes apenas para situações específicas. Por exemplo, é possível estender classes de sistema como `string`, o que torna seu novo código disponível para **qualquer** string. Se o seu código precisar executar uma lógica específica de domínio em um formato de string específico de domínio, um método de extensão não seria apropriado, pois sua presença confundiria os chamadores que trabalham com outras strings no sistema.

**A lista a seguir contém recursos básicos e propriedades dos métodos de extensão**

1. Deve ser um método estático.
2. Deve estar localizado em uma classe estática.
3. Ele usa a palavra-chave "this" como o primeiro parâmetro com um tipo em .NET e esse método será chamado por uma determinada instância de tipo no lado do cliente.
4. Também é mostrado pelo VS intellisense. Quando pressionamos o ponto `.` após uma instância de tipo, ele vem em VS intellisense.
5. Um método de extensão deve estar no mesmo namespace em que é usado ou você precisa importar o namespace da classe por meio de uma instrução using.
6. Você pode dar qualquer nome para a classe que tem um método de extensão, mas a classe deve ser estática.
7. Se você deseja adicionar novos métodos a um tipo e não possui o código-fonte para ele, a solução é usar e implementar métodos de extensão desse tipo.
8. Se você criar métodos de extensão que tenham os mesmos métodos de assinatura do tipo que está estendendo, os métodos de extensão nunca serão chamados.


[1]: http://stackoverflow.com/q/1226189
[2]: http://stackoverflow.com/questions/2520446/how-do-you-manage-the-namespaces-of-your-extension-methods

## Métodos de extensão - visão geral
Os métodos de extensão foram introduzidos no C# 3.0. Os métodos de extensão estendem e adicionam comportamento a tipos existentes sem criar um novo tipo derivado, recompilar ou modificar o tipo original. *Eles são especialmente úteis quando você não pode modificar a fonte de um tipo que deseja aprimorar.* Métodos de extensão podem ser criados para tipos de sistema, tipos definidos por terceiros e tipos que você mesmo definiu. O método de extensão pode ser invocado como se fosse um método membro do tipo original. Isso permite o **Method Chaining** usado para implementar uma **Interface Fluent**.

Um método de extensão é criado adicionando um **método estático** a uma **classe estática** que é diferente do tipo original que está sendo estendido. A classe estática que contém o método de extensão geralmente é criada com o único propósito de conter métodos de extensão.

Os métodos de extensão usam um primeiro parâmetro especial que designa o tipo original que está sendo estendido. Este primeiro parâmetro é decorado com a palavra-chave `this` (que constitui um uso especial e distinto de `this` em C#&mdash;deve ser entendido como diferente do uso de `this` que permite fazer referência a membros da instância atual do objeto) .

No exemplo a seguir, o tipo original que está sendo estendido é a classe `string`. `String` foi estendido por um método `Shorten()`, que fornece a funcionalidade adicional de encurtamento. A classe estática `StringExtensions` foi criada para conter o método de extensão. O método de extensão `Shorten()` mostra que é uma extensão de `string` através do primeiro parâmetro especialmente marcado. Para mostrar que o método `Shorten()` estende `string`, o primeiro parâmetro é marcado com `this`. Portanto, a assinatura completa do primeiro parâmetro é `this string text`, onde `string` é o tipo original que está sendo estendido e `text` é o nome do parâmetro escolhido.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

    class Program
    {
        static void Main()
        {
            // This calls method String.ToUpper()
            var myString = "Hello World!".ToUpper();

            // This calls the extension method StringExtensions.Shorten()
            var newString = myString.Shorten(5); 

            // It is worth noting that the above call is purely syntactic sugar
            // and the assignment below is functionally equivalent
            var newString2 = StringExtensions.Shorten(myString, 5);
        }
    }

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/uiPhpP)

-------------------------------------------------- ----------------------------------

O objeto passado como o *primeiro argumento de um método de extensão* (que é acompanhado pela palavra-chave `this`) é a instância que o método de extensão é chamado.

Por exemplo, quando este código é executado:

    "some string".Shorten(5);

Os valores dos argumentos são os seguintes:

    text: "some string"
    length: 5

*Observe que os métodos de extensão só podem ser usados ​​se estiverem no mesmo namespace que sua definição, se o namespace for importado explicitamente pelo código usando o método de extensão ou se a classe de extensão não tiver namespace.* As diretrizes do .NET framework recomendam colocando classes de extensão em seu próprio namespace. No entanto, isso pode levar a problemas de descoberta.

Isso resulta em nenhum conflito entre os métodos de extensão e as bibliotecas que estão sendo usadas, a menos que namespaces que possam entrar em conflito sejam explicitamente puxados. Por exemplo [LINQ Extensions][1]:
    
    using System.Linq; // Allows use of extension methods from the System.Linq namespace

    class Program
    {
        static void Main()
        {
            var ints = new int[] {1, 2, 3, 4};

            // Call Where() extension method from the System.Linq namespace
            var even = ints.Where(x => x % 2 == 0); 
        }
    }

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/IF223c)

-------------------------------------------------- ----------------------------------

Desde o C# 6.0, também é possível colocar uma diretiva `using static` na _class_ contendo os métodos de extensão. Por exemplo, `usando estático System.Linq.Enumerable;`. Isso torna os métodos de extensão dessa classe específica disponíveis sem trazer outros tipos do mesmo namespace para o escopo.

-------------------------------------------------- ----------------------------------

Quando um método de classe com a mesma assinatura está disponível, o compilador o prioriza sobre a chamada do método de extensão. Por exemplo:

    class Test
    {
       public void Hello()
       {
           Console.WriteLine("From Test");
       }
    }

    static class TestExtensions
    {
        public static void Hello(this Test test)
        {
            Console.WriteLine("From extension method");
        }
    }

    class Program
    {
        static void Main()
        {
            Test t = new Test();
            t.Hello(); // Prints "From Test"
        }
    }

<!-- versão final if -->

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/fI3sCJ)

-------------------------------------------------- ----------------------------------

Observe que, se houver duas funções de extensão com a mesma assinatura e uma delas estiver no mesmo namespace, essa será priorizada. Por outro lado, se ambos forem acessados ​​por `using`, ocorrerá um erro de tempo de compilação com a mensagem:
>**A chamada é ambígua entre os seguintes métodos ou propriedades**


-------------------------------------------------- ----------------------------------


Observe que a conveniência sintática de chamar um método de extensão via `originalTypeInstance.ExtensionMethod()` é uma conveniência opcional. O método também pode ser chamado da maneira tradicional, de modo que o primeiro parâmetro especial seja usado como parâmetro para o método.

Ou seja, ambos os seguintes trabalhos:

    //Calling as though method belongs to string--it seamlessly extends string
    String s = "Hello World";
    s.Shorten(5);  
    
    //Calling as a traditional static method with two parameters
    StringExtensions.Shorten(s, 5);

[1]: https://www.wikiod.com/pt/docs/c%23/68/linq-queries

## Verificação de nulos
Os métodos de extensão são métodos estáticos que se comportam como métodos de instância. No entanto, ao contrário do que acontece ao chamar um método de instância em uma referência `null`, quando um método de extensão é chamado com uma referência `null`, ele não lança uma [`NullReferenceException`][1]. Isso pode ser bastante útil em alguns cenários.

Por exemplo, considere a seguinte classe estática:

    public static class StringExtensions
    {
        public static string EmptyIfNull(this string text)
        {
            return text ?? String.Empty;
        }

        public static string NullIfEmpty(this string text)
        {
            return String.Empty == text ? null : text;
        }
    }

<!-- separado -->

    string nullString = null;
    string emptyString = nullString.EmptyIfNull();// will return ""
    string anotherNullString = emptyString.NullIfEmpty(); // will return null

[Demonstração ao vivo no .NET Fiddle][2]

[1]: https://msdn.microsoft.com/en-us/library/system.nullreferenceexception(v=vs.110).aspx
[2]: https://dotnetfiddle.net/jNQWqg

## Usando explicitamente um método de extensão
Os métodos de extensão também podem ser usados ​​como métodos de classe estática comuns. Essa forma de chamar um método de extensão é mais detalhada, mas é necessária em alguns casos.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

Uso:

    var newString = StringExtensions.Shorten("Hello World", 5);

# Quando chamar métodos de extensão como métodos estáticos

Ainda existem cenários em que você precisaria usar um método de extensão como um método estático:

* Resolvendo conflitos com um método de membro. Isso pode acontecer se uma nova versão de uma biblioteca introduzir um novo método de membro com a mesma assinatura. Nesse caso, o método membro será preferido pelo compilador.
* Resolvendo conflitos com outro método de extensão com a mesma assinatura. Isso pode acontecer se duas bibliotecas incluirem métodos de extensão semelhantes e namespaces de ambas as classes com métodos de extensão forem usados ​​no mesmo arquivo.
* Passando o método de extensão como um grupo de métodos no parâmetro delegate.
* Fazendo sua própria ligação através de `Reflection`.
* Usando o método de extensão na janela Immediate no Visual Studio.

# Usando estático

Se uma diretiva `using static` for usada para trazer membros estáticos de uma classe estática para o escopo global, os métodos de extensão serão ignorados. Exemplo:

    using static OurNamespace.StringExtensions; // refers to class in previous example

    // OK: extension method syntax still works.
    "Hello World".Shorten(5);
    // OK: static method syntax still works.
    OurNamespace.StringExtensions.Shorten("Hello World", 5);
    // Compile time error: extension methods can't be called as static without specifying class.
    Shorten("Hello World", 5);

Se você remover o modificador `this` do primeiro argumento do método `Shorten`, a última linha será compilada.


## Métodos de extensão só podem ver membros públicos (ou internos) da classe estendida
    public class SomeClass
    {
        public void DoStuff()
        {
            
        }

        protected void DoMagic()
        {
            
        }
    }

    public static class SomeClassExtensions
    {
        public static void DoStuffWrapper(this SomeClass someInstance)
        {
            someInstance.DoStuff(); // ok
        }

        public static void DoMagicWrapper(this SomeClass someInstance)
        {
            someInstance.DoMagic(); // compilation error
        }
    }

Os métodos de extensão são apenas um açúcar sintático e não são realmente membros da classe que eles estendem. Isso significa que eles não podem quebrar o encapsulamento &mdash;eles só têm acesso a campos, propriedades e métodos `públicos` (ou quando implementados no mesmo assembly, `interno`).

## Métodos de extensão genéricos
Assim como outros métodos, os métodos de extensão podem usar genéricos. Por exemplo:

    static class Extensions
    {
        public static bool HasMoreThanThreeElements<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.Take(4).Count() > 3;
        }
    }
e chamá-lo seria assim:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var hasMoreThanThreeElements = numbers.HasMoreThanThreeElements();

[Ver demonstração][1]

Da mesma forma para vários argumentos de tipo:

    public static TU GenericExt<T, TU>(this T obj)
    {
         TU ret = default(TU);
         // do some stuff with obj
         return ret;
    }

Chamar seria assim:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var result = numbers.GenericExt<IEnumerable<int>,String>();

[Ver demonstração][2]

Você também pode criar métodos de extensão para tipos parcialmente vinculados em vários tipos genéricos:

    class MyType<T1, T2>
    {
    }
    
    static class Extensions
    {
        public static void Example<T>(this MyType<int, T> test)
        {        
        }
    }

Chamar seria assim:

    MyType<int, string> t = new MyType<int, string>();
    t.Example();

[Ver demonstração][4]

Você também pode especificar restrições de tipo com [`where`][3] :

    public static bool IsDefault<T>(this T obj) where T : struct, IEquatable<T>
    {
         return EqualityComparer<T>.Default.Equals(obj, default(T));
    }

Código de chamada:

    int number = 5;
    var IsDefault = number.IsDefault();

[Ver demonstração][5]


[1]: https://dotnetfiddle.net/UlCa3i
[2]: https://dotnetfiddle.net/aMNO0X
[3]: https://www.wikiod.com/pt/docs/c%23/26/keywords/8137/where-type-constraints#t=201607221442171394675
[4]: https://dotnetfiddle.net/1FjUOH
[5]: https://dotnetfiddle.net/Jom3cS

## Métodos de extensão para encadeamento
Quando um método de extensão retorna um valor que tem o mesmo tipo de seu argumento `this`, ele pode ser usado para "encadear" uma ou mais chamadas de método com uma assinatura compatível. Isso pode ser útil para tipos selados e/ou primitivos e permite a criação das chamadas APIs "fluentes" se os nomes dos métodos forem lidos como linguagem humana natural.

    void Main()
    {
        int result = 5.Increment().Decrement().Increment(); 
        // result is now 6
    }
    
    public static class IntExtensions 
    {
        public static int Increment(this int number) {
            return ++number;
        }

        public static int Decrement(this int number) {
            return --number;
        }
    }

Ou assim

    void Main()
    {
        int[] ints = new[] { 1, 2, 3, 4, 5, 6};
        int[] a = ints.WhereEven();
        //a is { 2, 4, 6 };
        int[] b = ints.WhereEven().WhereGreaterThan(2);
        //b is { 4, 6 };
    }
    
    public static class IntArrayExtensions
    {
        public static int[] WhereEven(this int[] array)
        {
            //Enumerable.* extension methods use a fluent approach
            return array.Where(i => (i%2) == 0).ToArray();
        }
    
        public static int[] WhereGreaterThan(this int[] array, int value)
        {
            return array.Where(i => i > value).ToArray();
        }
    }

## Métodos de extensão com enumeração
Os métodos de extensão são úteis para adicionar funcionalidade a enumerações.

Um uso comum é implementar um método de conversão.

    public enum YesNo
    {
        Yes,
        No,
    }
    
    public static class EnumExtentions
    {
        public static bool ToBool(this YesNo yn)
        {
            return yn == YesNo.Yes;
        }
        public static YesNo ToYesNo(this bool yn)
        {
            return yn ? YesNo.Yes : YesNo.No;
        }
    }

Agora você pode converter rapidamente seu valor de enumeração para um tipo diferente. Neste caso, um bool.

    bool yesNoBool = YesNo.Yes.ToBool(); // yesNoBool == true
    YesNo yesNoEnum = false.ToYesNo();   // yesNoEnum == YesNo.No


Alternativamente, métodos de extensão podem ser usados ​​para adicionar métodos semelhantes a propriedades.

    public enum Element
    {
        Hydrogen,
        Helium,
        Lithium,
        Beryllium,
        Boron,
        Carbon,
        Nitrogen,
        Oxygen
        //Etc
    }

    public static class ElementExtensions
    {
        public static double AtomicMass(this Element element)
        {
            switch(element)
            {
                case Element.Hydrogen:  return 1.00794;
                case Element.Helium:    return 4.002602;
                case Element.Lithium:   return 6.941;
                case Element.Beryllium: return 9.012182;
                case Element.Boron:     return 10.811;
                case Element.Carbon:    return 12.0107;
                case Element.Nitrogen:  return 14.0067;
                case Element.Oxygen:    return 15.9994;
                //Etc
            }
            return double.Nan;
        }
    }

    var massWater = 2*Element.Hydrogen.AtomicMass() + Element.Oxygen.AtomicMass();

## Despacho de métodos de extensão com base no tipo estático
O tipo estático (tempo de compilação) é usado em vez do tipo dinâmico (tipo de tempo de execução) para corresponder aos parâmetros.

    public class Base 
    { 
        public virtual string GetName()
        {
            return "Base";
        }
    }

    public class Derived : Base
    { 
        public override string GetName()
        {
            return "Derived";
        }
    }

    public static class Extensions
    {
        public static string GetNameByExtension(this Base item)
        {
            return "Base";
        }

        public static string GetNameByExtension(this Derived item)
        {
            return "Derived";
        }
    }

    public static class Program   
    {
        public static void Main()
        {
            Derived derived = new Derived();
            Base @base = derived;

            // Use the instance method "GetName"
            Console.WriteLine(derived.GetName()); // Prints "Derived"
            Console.WriteLine(@base.GetName()); // Prints "Derived"

            // Use the static extension method "GetNameByExtension"
            Console.WriteLine(derived.GetNameByExtension()); // Prints "Derived"
            Console.WriteLine(@base.GetNameByExtension()); // Prints "Base"
        }
    }
[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/7BGp8o)

Além disso, o dispatch baseado no tipo estático não permite que um método de extensão seja chamado em um objeto `dinâmico`:

    public class Person
    {
        public string Name { get; set; }
    }
    
    public static class ExtenionPerson
    {
        public static string GetPersonName(this Person person)
        {
            return person.Name;
        }
    }
    
    dynamic person = new Person { Name = "Jon" };
    var name = person.GetPersonName(); // RuntimeBinderException is thrown

## Métodos de extensão em interfaces
Um recurso útil dos métodos de extensão é que você pode criar métodos comuns para uma interface. Normalmente uma interface não pode ter implementações compartilhadas, mas com métodos de extensão elas podem.

    public interface IVehicle
    {
        int MilesDriven { get; set; }
    }
    
    public static class Extensions
    {
        public static int FeetDriven(this IVehicle vehicle)
        {
            return vehicle.MilesDriven * 5028;
        }
    }

Neste exemplo, o método `FeetDriven` pode ser usado em qualquer `IVehicle`. Esta lógica neste método se aplicaria a todos os `IVehicle`s, então pode ser feito desta forma para que não tenha que haver um `FeetDriven` na definição de `IVehicle` que seria implementado da mesma forma para todos os filhos .

## Métodos de extensão não são compatíveis com código dinâmico.

    static class Program
    {
        static void Main()
        {
            dynamic dynamicObject = new ExpandoObject();
    
            string awesomeString = "Awesome";
    
            // Prints True
            Console.WriteLine(awesomeString.IsThisAwesome());
    
            dynamicObject.StringValue = awesomeString;
    
            // Prints True
            Console.WriteLine(StringExtensions.IsThisAwesome(dynamicObject.StringValue)); 
            
            // No compile time error or warning, but on runtime throws RuntimeBinderException
            Console.WriteLine(dynamicObject.StringValue.IsThisAwesome());
        }
    }
    
    static class StringExtensions
    {
        public static bool IsThisAwesome(this string value)
        {
            return value.Equals("Awesome");
        }
    }

> A razão [chamar métodos de extensão de código dinâmico] não funciona é porque em métodos de extensão de código não dinâmicos regulares funcionam fazendo uma pesquisa completa de todas as classes conhecidas pelo compilador para uma classe estática que tem um método de extensão que fósforos. A busca segue em ordem baseada no aninhamento do namespace e nas diretivas `using` disponíveis em cada namespace.
> 
> Isso significa que para obter uma invocação de método de extensão dinâmica resolvida corretamente, de alguma forma o DLR tem que saber *em tempo de execução* quais eram todos os aninhamentos de namespace e diretivas `using` *em seu código-fonte*. Não temos um mecanismo à mão para codificar todas essas informações no site da chamada. Consideramos inventar tal mecanismo, mas decidimos que era um custo muito alto e produzia muito risco de cronograma para valer a pena.

[Fonte](http://stackoverflow.com/a/5313149/1610754)

## Métodos de extensão em combinação com interfaces
É muito conveniente usar métodos de extensão com interfaces, pois a implementação pode ser armazenada fora da classe e tudo o que é necessário para adicionar alguma funcionalidade à classe é decorar a classe com a interface.

    public interface IInterface
    {
       string Do()
    }

    public static class ExtensionMethods{
        public static string DoWith(this IInterface obj){
          //does something with IInterface instance
        }
    }

    public class Classy : IInterface
    {
       // this is a wrapper method; you could also call DoWith() on a Classy instance directly,
       // provided you import the namespace containing the extension method
       public Do(){
           return this.DoWith();
       }
    }


use como:

     var classy = new Classy();
     classy.Do(); // will call the extension
     classy.DoWith(); // Classy implements IInterface so it can also be called this way

## Extensões e interfaces juntas habilitam código DRY e funcionalidade tipo mixin
Os métodos de extensão permitem que você simplifique suas definições de interface incluindo apenas a funcionalidade principal necessária na própria interface e permitindo que você defina métodos de conveniência e sobrecargas como métodos de extensão. Interfaces com menos métodos são mais fáceis de implementar em novas classes. Manter sobrecargas como extensões, em vez de incluí-las na interface, evita que você copie código clichê em cada implementação, ajudando a manter seu código DRY. Na verdade, isso é semelhante ao padrão mixin que o C# não suporta.

As extensões de `System.Linq.Enumerable` para `IEnumerable<T>` são um ótimo exemplo disso. `IEnumerable<T>` requer apenas que a classe de implementação implemente dois métodos: `GetEnumerator()` genérico e não genérico. Mas `System.Linq.Enumerable` fornece inúmeros utilitários úteis como extensões que permitem o consumo conciso e claro de `IEnumerable<T>`.

O seguinte é uma interface muito simples com sobrecargas de conveniência fornecidas como extensões.

    public interface ITimeFormatter
    {
       string Format(TimeSpan span);
    }

    public static class TimeFormatter
    {
        // Provide an overload to *all* implementers of ITimeFormatter.
        public static string Format(
            this ITimeFormatter formatter,
            int millisecondsSpan)
            => formatter.Format(TimeSpan.FromMilliseconds(millisecondsSpan));
    }

    // Implementations only need to provide one method. Very easy to
    // write additional implementations.
    public class SecondsTimeFormatter : ITimeFormatter
    {
       public string Format(TimeSpan span)
       {
           return $"{(int)span.TotalSeconds}s";
       }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var formatter = new SecondsTimeFormatter();
            // Callers get two method overloads!
            Console.WriteLine($"4500ms is rougly {formatter.Format(4500)}");
            var span = TimeSpan.FromSeconds(5);
            Console.WriteLine($"{span} is formatted as {formatter.Format(span)}");
        }
    }


## IList<T> Exemplo de método de extensão: comparando 2 listas
Você pode usar o seguinte método de extensão para comparar o conteúdo de duas instâncias IList< T > do mesmo tipo.

Por padrão, os itens são comparados com base em sua ordem na lista e nos próprios itens, passando false para o parâmetro `isOrdered` comparará apenas os próprios itens, independentemente de sua ordem.

Para que este método funcione, o tipo genérico (`T`) deve substituir os métodos `Equals` e `GetHashCode`.

**Uso:**

    List<string> list1 = new List<string> {"a1", "a2", null, "a3"};
    List<string> list2 = new List<string> {"a1", "a2", "a3", null};

    list1.Compare(list2);//this gives false
    list1.Compare(list2, false);//this gives true. they are equal when the order is disregarded

**Método:**

    public static bool Compare<T>(this IList<T> list1, IList<T> list2, bool isOrdered = true) 
    {
        if (list1 == null && list2 == null)
            return true;
        if (list1 == null || list2 == null || list1.Count != list2.Count)
            return false;

        if (isOrdered)
        {
            for (int i = 0; i < list2.Count; i++)
            {
                var l1 = list1[i]; 
                var l2 = list2[i];
                if (
                     (l1 == null && l2 != null) || 
                     (l1 != null && l2 == null) || 
                     (!l1.Equals(l2)))
                {
                        return false;
                }
            }
            return true;
        }
        else
        {
            List<T> list2Copy = new List<T>(list2);
            //Can be done with Dictionary without O(n^2)
            for (int i = 0; i < list1.Count; i++)
            {
                if (!list2Copy.Remove(list1[i]))
                    return false;
            }
            return true;
        }
    }

## Métodos de extensão como wrappers fortemente tipados
Os métodos de extensão podem ser usados ​​para escrever wrappers fortemente tipados para objetos semelhantes a dicionários. Por exemplo, um cache, `HttpContext.Items` at cetera...

    public static class CacheExtensions
    {
        public static void SetUserInfo(this Cache cache, UserInfo data) => 
            cache["UserInfo"] = data;

        public static UserInfo GetUserInfo(this Cache cache) => 
            cache["UserInfo"] as UserInfo;
    }

Essa abordagem elimina a necessidade de usar literais de string como chaves em toda a base de código, bem como a necessidade de converter para o tipo necessário durante a operação de leitura. No geral, ele cria uma maneira mais segura e fortemente tipada de interagir com objetos de tipo livre como Dicionários.

## Usando métodos de extensão para criar belas classes mapeadoras
Podemos criar classes mapeadoras melhores com métodos de extensão,
Suponha que se eu tiver algumas classes DTO como

     public class UserDTO
     {
            public AddressDTO Address { get; set; }
     }
    
     public class AddressDTO
     {
            public string Name { get; set; }
     }

e eu preciso mapear para as classes de modelo de exibição correspondentes

    public class UserViewModel
    {
        public AddressViewModel Address { get; set; }
    }
    
    public class AddressViewModel
    {
        public string Name { get; set; }
    }

então eu posso criar minha classe mapeadora como abaixo

    public static class ViewModelMapper
    {
          public static UserViewModel ToViewModel(this UserDTO user)
          {
                return user == null ?
                    null :
                    new UserViewModel()
                    {
                        Address = user.Address.ToViewModel()
                        // Job = user.Job.ToViewModel(),
                        // Contact = user.Contact.ToViewModel() .. and so on
                    };
          }
    
          public static AddressViewModel ToViewModel(this AddressDTO userAddr)
          {
                return userAddr == null ?
                    null :
                    new AddressViewModel()
                    {
                        Name = userAddr.Name
                    };
          }
    }

Então, finalmente, posso invocar meu mapeador como abaixo

        UserDTO userDTOObj = new UserDTO() {
                Address = new AddressDTO() {
                    Name = "Address of the user"
                }
            };

        UserViewModel user = userDTOObj.ToViewModel(); // My DTO mapped to Viewmodel


A beleza aqui é que todo o método de mapeamento tem um nome comum (ToViewModel) e podemos reutilizá-lo de várias maneiras

## Usando métodos de extensão para criar novos tipos de coleção (por exemplo, DictList)
Você pode criar métodos de extensão para melhorar a usabilidade de coleções aninhadas como um `Dictionary` com um valor `List<T>`.

Considere os seguintes métodos de extensão:

    public static class DictListExtensions
    {
        public static void Add<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
                where TCollection : ICollection<TValue>, new()
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                list = new TCollection();
                dict.Add(key, list);
            }

            list.Add(value);
        }

        public static bool Remove<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
            where TCollection : ICollection<TValue>
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                return false;
            }

            var ret = list.Remove(value);
            if (list.Count == 0)
            {
                dict.Remove(key);
            }
            return ret;
        }
    }

você pode usar os métodos de extensão da seguinte forma:

    var dictList = new Dictionary<string, List<int>>();

    dictList.Add("example", 5);
    dictList.Add("example", 10);
    dictList.Add("example", 15);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 5, 10, 15

    dictList.Remove("example", 5);
    dictList.Remove("example", 10);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 15
    
    dictList.Remove("example", 15);
    
    Console.WriteLine(dictList.ContainsKey("example")); // False

[Ver demonstração](https://dotnetfiddle.net/UbdQuC)

## Métodos de extensão para lidar com casos especiais

Os métodos de extensão podem ser usados ​​para "ocultar" o processamento de regras de negócios deselegantes que, de outra forma, exigiriam sobrecarregar uma função de chamada com instruções if/then. Isso é semelhante e análogo ao tratamento de nulos com métodos de extensão. Por exemplo,

    public static class CakeExtensions
    {
        public static Cake EnsureTrueCake(this Cake cake)
        {
            //If the cake is a lie, substitute a cake from grandma, whose cakes aren't as tasty but are known never to be lies. If the cake isn't a lie, don't do anything and return it.
            return CakeVerificationService.IsCakeLie(cake) ? GrandmasKitchen.Get1950sCake() : cake;
        }
    }

<!-- separado -->

    Cake myCake = Bakery.GetNextCake().EnsureTrueCake();
    myMouth.Eat(myCake);//Eat the cake, confident that it is not a lie.


## Usando métodos de extensão com métodos estáticos e retornos de chamada
Considere usar métodos de extensão como funções que encapsulam outro código, aqui está um ótimo exemplo que usa um método estático e um método de extensão para encapsular a construção Try Catch. Faça seu código à prova de balas...

    using System;
    using System.Diagnostics;
    
    namespace Samples
    {
        /// <summary>
        /// Wraps a try catch statement as a static helper which uses 
        /// Extension methods for the exception
        /// </summary>
        public static class Bullet
        {
            /// <summary>
            /// Wrapper for Try Catch Statement
            /// </summary>
            /// <param name="code">Call back for code</param>
            /// <param name="error">Already handled and logged exception</param>
            public static void Proof(Action code, Action<Exception> error)
            {
                try
                {
                    code();
                }
                catch (Exception iox)
                {
                    //extension method used here
                    iox.Log("BP2200-ERR-Unexpected Error");
                    //callback, exception already handled and logged
                    error(iox);
                }
            }
            /// <summary>
            /// Example of a logging method helper, this is the extension method
            /// </summary>
            /// <param name="error">The Exception to log</param>
            /// <param name="messageID">A unique error ID header</param>
            public static void Log(this Exception error, string messageID)
            {
                Trace.WriteLine(messageID);
                Trace.WriteLine(error.Message);
                Trace.WriteLine(error.StackTrace);
                Trace.WriteLine("");
            }
        }
        /// <summary>
        /// Shows how to use both the wrapper and extension methods.
        /// </summary>
        public class UseBulletProofing
        {
            public UseBulletProofing()
            {
                var ok = false;
                var result = DoSomething();
                if (!result.Contains("ERR"))
                {
                    ok = true;
                    DoSomethingElse();
                }
            }
    
            /// <summary>
            /// How to use Bullet Proofing in your code.
            /// </summary>
            /// <returns>A string</returns>
            public string DoSomething()
            {
                string result = string.Empty;
                //Note that the Bullet.Proof method forces this construct.
                Bullet.Proof(() =>
                {
                    //this is the code callback
                    result = "DST5900-INF-No Exceptions in this code";
                }, error =>
                {
                    //error is the already logged and handled exception
                    //determine the base result
                    result = "DTS6200-ERR-An exception happened look at console log";
                    if (error.Message.Contains("SomeMarker"))
                    {
                        //filter the result for Something within the exception message
                        result = "DST6500-ERR-Some marker was found in the exception";
                    }
                });
                return result;
            }
    
            /// <summary>
            /// Next step in workflow
            /// </summary>
            public void DoSomethingElse()
            {
                //Only called if no exception was thrown before
            }
        }
    }

