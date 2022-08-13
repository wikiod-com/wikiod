---
title: "Genéricos"
slug: "genericos"
draft: false
images: []
weight: 9216
type: docs
toc: true
---

## Sintaxe
- `public void SomeMethod <T> () { }`
- `public void SomeMethod<T, V>() { }`
- `public T SomeMethod<T>(IEnumerable<T> sequence) { ... }`
- `public void SomeMethod<T>() onde T : new() { }`
- `public void SomeMethod<T, V>() onde T : new() onde V : struct { }`
- `public void SomeMethod<T>() onde T: IDisposable { }`
- `public void SomeMethod<T>() onde T: Foo { }`
- `public class MinhaClasse<T> { public T Data {get; definir; } }`

## Parâmetros
| Parâmetro(s) | Descrição |
|---|---|
| T, V | Espaços reservados de tipo para declarações genéricas |



Os genéricos em C# são suportados até o tempo de execução: os tipos genéricos criados com C# terão sua semântica genérica preservada mesmo depois de compilados para [CIL][1].


Isso significa efetivamente que, em C#, é possível refletir sobre tipos genéricos e vê-los como foram declarados ou verificar se um objeto é uma instância de um tipo genérico, por exemplo. Isso contrasta com [type erasure][2], onde as informações de tipo genérico são removidas durante a compilação. Também está em contraste com a abordagem de modelo para genéricos, em que vários tipos genéricos concretos se tornam vários tipos não genéricos em tempo de execução e quaisquer metadados necessários para instanciar ainda mais as definições de tipos genéricos originais são perdidos.

Tenha cuidado, no entanto, ao refletir sobre tipos genéricos: os nomes dos tipos genéricos serão alterados na compilação, substituindo os colchetes angulares e os nomes dos parâmetros de tipo por um acento grave seguido pelo número de parâmetros de tipo genérico. Assim, um `Dictionary<TKey, Tvalue>` será traduzido para ``Dictionary`2``.


[1]: https://en.wikipedia.org/wiki/Common_Intermediate_Language
[2]: https://en.wikipedia.org/wiki/Type_erasure

## Inferência de tipo implícita (métodos)
Ao passar argumentos formais para um método genérico, os argumentos de tipo genérico relevantes geralmente podem ser inferidos implicitamente. Se todos os tipos genéricos puderem ser inferidos, especificá-los na sintaxe é opcional.

Considere o seguinte método genérico. Ele tem um parâmetro formal e um parâmetro de tipo genérico. Existe uma relação muito óbvia entre eles -- o tipo passado como argumento para o parâmetro de tipo genérico deve ser o mesmo que o tipo de tempo de compilação do argumento passado para o parâmetro formal.

    void M<T>(T obj)
    {
    }

Essas duas chamadas são equivalentes:

    M<object>(new object());
    M(new object());

Essas duas chamadas também são equivalentes:

    M<string>("");
    M("");

E assim são estas três chamadas:

    M<object>("");
    M((object) "");
    M("" as object);

---

Observe que, se pelo menos um argumento de tipo não puder ser inferido, todos eles deverão ser especificados.

Considere o seguinte método genérico. O primeiro argumento de tipo genérico é igual ao tipo do argumento formal. Mas não existe tal relação para o segundo argumento de tipo genérico. Portanto, o compilador não tem como inferir o segundo argumento de tipo genérico em qualquer chamada para esse método.

    void X<T1, T2>(T1 obj)
    {
    }

Isso não funciona mais:

    X("");

Isso também não funciona, porque o compilador não tem certeza se estamos especificando o primeiro ou o segundo parâmetro genérico (ambos seriam válidos como `object`):

    X<object>("");

Somos obrigados a digitar os dois, assim:

    X<string, object>("");

## Inferência de tipo (classes)
Os desenvolvedores podem ser pegos pelo fato de que a inferência de tipo *não funciona* para construtores:

    class Tuple<T1,T2>
    {
       public Tuple(T1 value1, T2 value2)
       {
       }
    }

    var x = new Tuple(2, "two");              // This WON'T work...
    var y = new Tuple<int, string>(2, "two"); // even though the explicit form will.

A primeira maneira de criar instância sem especificar explicitamente os parâmetros de tipo causará um erro de tempo de compilação que diria:
>Usar o tipo genérico 'Tuple<T1, T2>' requer 2 argumentos de tipo

Uma solução comum é adicionar um método auxiliar em uma classe estática:

    static class Tuple
    {
        public static Tuple<T1, T2> Create<T1, T2>(T1 value1, T2 value2)
        {
             return new Tuple<T1, T2>(value1, value2);
        }
    }

    var x = Tuple.Create(2, "two");  // This WILL work...

## Usando método genérico com uma interface como tipo de restrição.
Este é um exemplo de como usar o tipo genérico TFood dentro do método Eat<TFood> na classe Animal

    public interface IFood
    {
        void EatenBy(Animal animal);
    }
    
    public class Grass: IFood
    {
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Grass was eaten by: {0}", animal.Name);
        }
    }
    
    public class Animal
    {
        public string Name { get; set; }
    
        public void Eat<TFood>(TFood food)
            where TFood : IFood
        {
            food.EatenBy(this);
        }
    }
    
    public class Carnivore : Animal
    {
        public Carnivore()
        {
            Name = "Carnivore";
        }
    }
    
    public class Herbivore : Animal, IFood
    {
        public Herbivore()
        {
            Name = "Herbivore";
        }
        
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Herbivore was eaten by: {0}", animal.Name);
        }
    }

Você pode chamar o método Eat<TFood> assim:

    var grass = new Grass();        
    var sheep = new Herbivore();
    var lion = new Carnivore();
        
    sheep.Eat(grass);
    //Output: Grass was eaten by: Herbivore

    lion.Eat(sheep);
    //Output: Herbivore was eaten by: Carnivore

Neste caso, se você tentar chamar:
   
    sheep.Eat(lion);

Não será possível porque o objeto lion não implementa a interface IFood. A tentativa de fazer a chamada acima gerará um erro de compilador: "O tipo 'Carnivore' não pode ser usado como parâmetro de tipo 'TFood' no tipo genérico ou método 'Animal.Eat<TFood>(TFood)'. Não há referência implícita conversão de 'Carnivore' para 'IFood'."

## Restrições de tipo (nova palavra-chave)
Usando a restrição `new()`, é possível impor parâmetros de tipo para definir um construtor vazio (padrão).

    class Foo
    {
        public Foo () { }
    }

    class Bar
    {
        public Bar (string s) { ... }
    }

    class Factory<T>
        where T : new()
    {
        public T Create()
        {
            return new T();
        }
    }

    Foo f = new Factory<Foo>().Create(); // Valid.
    Bar b = new Factory<Bar>().Create(); // Invalid, Bar does not define a default/empty constructor.

A segunda chamada para `Create()` dará um erro de tempo de compilação com a seguinte mensagem:
>'Bar' deve ser um tipo não abstrato com um construtor público sem parâmetros para usá-lo como parâmetro 'T' no tipo genérico ou método 'Factory<T>'

Não há restrição para um construtor com parâmetros, apenas construtores sem parâmetros são suportados.

## Restrições de tipo (classes e interfaces)
As restrições de tipo são capazes de forçar um parâmetro de tipo a implementar uma determinada interface ou classe.

    interface IType;
    interface IAnotherType;

    // T must be a subtype of IType
    interface IGeneric<T>
        where T : IType
    {
    }

    // T must be a subtype of IType
    class Generic<T>
        where T : IType
    {
    }

    class NonGeneric
    {
        // T must be a subtype of IType
        public void DoSomething<T>(T arg)
            where T : IType
        {
        }
    }

    // Valid definitions and expressions:
    class Type : IType { }
    class Sub : IGeneric<Type> { }
    class Sub : Generic<Type> { }
    new NonGeneric().DoSomething(new Type());

    // Invalid definitions and expressions:
    class AnotherType : IAnotherType { }
    class Sub : IGeneric<AnotherType> { }
    class Sub : Generic<AnotherType> { }
    new NonGeneric().DoSomething(new AnotherType());

Sintaxe para várias restrições:

    class Generic<T, T1>
        where T : IType 
        where T1 : Base, new()
    {
    }

As restrições de tipo funcionam da mesma forma que a herança, pois é possível especificar várias interfaces como restrições no tipo genérico, mas apenas uma classe:

    class A { /* ... */ }
    class B { /* ... */ }

    interface I1 { }
    interface I2 { }

    class Generic<T>
        where T : A, I1, I2
    {
    }

    class Generic2<T>
        where T : A, B //Compilation error
    {
    }

Outra regra é que a classe deve ser adicionada como primeira restrição e depois as interfaces:

    class Generic<T>
        where T : A, I1
    {
    }

    class Generic2<T>
        where T : I1, A //Compilation error
    {
    }

Todas as restrições declaradas devem ser satisfeitas simultaneamente para que uma instanciação genérica específica funcione. Não há como especificar dois ou mais conjuntos alternativos de restrições.

## Refletindo sobre parâmetros de tipo
O operador `typeof` funciona em parâmetros de tipo.

    class NameGetter<T>
    {
        public string GetTypeName()
        {
            return typeof(T).Name;
        }
    }

## Covariância
Quando um `IEnumerable<T>` é um subtipo de um `IEnumerable<T1>` diferente? Quando `T` é um subtipo de `T1`. `IEnumerable` é _covariante_ em seu parâmetro `T`, o que significa que o relacionamento de subtipo `IEnumerable` vai na _mesma direção_ que `T`'s.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
    IEnumerable<Animal> animals = dogs;  // IEnumerable<Dog> is a subtype of IEnumerable<Animal>
    // dogs = animals;  // Compilation error - IEnumerable<Animal> is not a subtype of IEnumerable<Dog>

Uma instância de um tipo genérico covariante com um determinado parâmetro de tipo é implicitamente conversível para o mesmo tipo genérico com um parâmetro de tipo menos derivado.

Esta relação é válida porque `IEnumerable` _produz_ `T`s mas não os consome. Um objeto que produz `Dog`s pode ser usado como se produzisse `Animal`s.

Parâmetros de tipo covariante são declarados usando a palavra-chave `out`, pois o parâmetro deve ser usado apenas como _output_.

    interface IEnumerable<out T> { /* ... */ }

Um parâmetro de tipo declarado como covariante pode não aparecer como entrada.

    interface Bad<out T>
    {
        void SetT(T t);  // type error
    }

Aqui está um exemplo completo:

    using NUnit.Framework;
    
    namespace ToyStore
    {
       enum Taste { Bitter, Sweet };
    
       interface IWidget
       {
          int Weight { get; }
       }
    
       interface IFactory<out TWidget>
           where TWidget : IWidget
       {
          TWidget Create();
       }
    
       class Toy : IWidget
       {
          public int Weight { get; set; }
          public Taste Taste { get; set; }
       }
    
       class ToyFactory : IFactory<Toy>
       {
          public const int StandardWeight = 100;
          public const Taste StandardTaste = Taste.Sweet;

          public Toy Create() { return new Toy { Weight = StandardWeight, Taste = StandardTaste }; }
       }
    
       [TestFixture]
       public class GivenAToyFactory
       {
          [Test]
          public static void WhenUsingToyFactoryToMakeWidgets()
          {
             var toyFactory = new ToyFactory();
    
             //// Without out keyword, note the verbose explicit cast:
             // IFactory<IWidget> rustBeltFactory = (IFactory<IWidget>)toyFactory;
    
             // covariance: concrete being assigned to abstract (shiny and new)
             IFactory<IWidget> widgetFactory = toyFactory;
             IWidget anotherToy = widgetFactory.Create();
             Assert.That(anotherToy.Weight, Is.EqualTo(ToyFactory.StandardWeight)); // abstract contract
             Assert.That(((Toy)anotherToy).Taste, Is.EqualTo(ToyFactory.StandardTaste)); // concrete contract
          }
       }
    }
    

## Contravariância
Quando um `IComparer<T>` é um subtipo de um `IComparer<T1>` diferente? Quando `T1` é um subtipo de `T`. `IComparer` é _contravariante_ em seu parâmetro `T`, o que significa que o relacionamento de subtipo `IComparer` vai na _direção oposta_ como `T`'s.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IComparer<Animal> animalComparer = /* ... */;
    IComparer<Dog> dogComparer = animalComparer;  // IComparer<Animal> is a subtype of IComparer<Dog>
    // animalComparer = dogComparer;  // Compilation error - IComparer<Dog> is not a subtype of IComparer<Animal>

Uma instância de um tipo genérico contravariante com um determinado parâmetro de tipo é implicitamente conversível para o mesmo tipo genérico com um parâmetro de tipo mais derivado.

Esta relação é válida porque `IComparer` _consome_ `T`s mas não os produz. Um objeto que pode comparar quaisquer dois `Animal`s pode ser usado para comparar dois `Dog`s.

Parâmetros de tipo contravariante são declarados usando a palavra-chave `in`, porque o parâmetro deve ser usado apenas como _input_.

    interface IComparer<in T> { /* ... */ }

Um parâmetro de tipo declarado como contravariante pode não aparecer como saída.

    interface Bad<in T>
    {
        T GetT();  // type error
    }

## Invariância
`IList<T>` nunca é um subtipo de um `IList<T1>` diferente. `IList` é _invariante_ em seu parâmetro de tipo.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }
    
    IList<Dog> dogs = new List<Dog>();
    IList<Animal> animals = dogs;  // type error

Não há relacionamento de subtipo para listas porque você pode colocar valores em uma lista _e_ retirar valores de uma lista.

Se `IList` fosse covariante, você poderia adicionar itens do _subtipo errado_ a uma determinada lista.

    IList<Animal> animals = new List<Dog>();  // supposing this were allowed...
    animals.Add(new Giraffe());  // ... then this would also be allowed, which is bad!

Se `IList` fosse contravariante, você poderia extrair valores do subtipo errado de uma determinada lista.

    IList<Dog> dogs = new List<Animal> { new Dog(), new Giraffe() };  // if this were allowed...
    Dog dog = dogs[1];  // ... then this would be allowed, which is bad!

Parâmetros de tipo invariável são declarados omitindo as palavras-chave `in` e `out`.

    interface IList<T> { /* ... */ }

## Interfaces variantes
As interfaces podem ter parâmetros de tipo variante.

    interface IEnumerable<out T>
    {
        // ...
    }
    interface IComparer<in T>
    {
        // ...
    }

mas classes e estruturas podem não

    class BadClass<in T1, out T2>  // not allowed
    {
    }
    
    struct BadStruct<in T1, out T2>  // not allowed
    {
    }

nem declarações de métodos genéricos

    class MyClass
    {
        public T Bad<out T, in T1>(T1 t1)  // not allowed
        {
            // ...
        }
    }

O exemplo abaixo mostra várias declarações de variação na mesma interface

    interface IFoo<in T1, out T2, T3>
    //  T1 : Contravariant type
    //  T2 : Covariant type 
    //  T3 : Invariant type
    {
        // ...
    }
    
    IFoo<Animal, Dog, int> foo1 = /* ... */;
    IFoo<Dog, Animal, int> foo2 = foo1;  
    // IFoo<Animal, Dog, int> is a subtype of IFoo<Dog, Animal, int>



## Verificando a igualdade de valores genéricos.
Se a lógica da classe ou método genérico exigir a verificação da igualdade de valores com tipo genérico, use `EqualityComparer<TType>.Default` [propriedade][1]:


    public void Foo<TBar>(TBar arg1, TBar arg2)
    {
        var comparer = EqualityComparer<TBar>.Default;
        if (comparer.Equals(arg1,arg2)
        {
            ...
        }
    }

Esta abordagem é melhor do que simplesmente chamar o método `Object.Equals()`, porque a implementação do comparador padrão verifica se o tipo `TBar` implementa `IEquatale<TBar>` [interface][2] e se sim, chama `IEquatable<TBar> .Equals(TBar other)`. Isso permite evitar boxing/unboxing de tipos de valor.


[1]: https://msdn.microsoft.com/en-us/library/ms224763(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx

## Parâmetros de tipo (interfaces)
Declaração:

    interface IMyGenericInterface<T1, T2, T3, ...> { ... }

Uso (na herança):

    class ClassA<T1, T2, T3> : IMyGenericInterface<T1, T2, T3> { ... }

    class ClassB<T1, T2> : IMyGenericInterface<T1, T2, int> { ... }

    class ClassC<T1> : IMyGenericInterface<T1, char, int> { ... }

    class ClassD : IMyGenericInterface<bool, char, int> { ... }

Uso (como o tipo de um parâmetro):

    void SomeMethod(IMyGenericInterface<int, char, bool> arg) { ... }

## Delegados variantes
Os delegados podem ter parâmetros de tipo variante.

    delegate void Action<in T>(T t);    // T is an input
    delegate T Func<out T>();           // T is an output
    delegate T2 Func<in T1, out T2>();  // T1 is an input, T2 is an output

Isso decorre do [Princípio da Substituição de Liskov][1], que afirma (entre outras coisas) que um método D pode ser considerado mais derivado do que um método B se:

- D tem um tipo de retorno derivado igual ou mais que B
- D tem tipos de parâmetros correspondentes iguais ou mais gerais do que B

Portanto, as seguintes atribuições são todas seguras para o tipo:

    Func<object, string> original = SomeMethod;
    Func<object, object> d1 = original;
    Func<string, string> d2 = original;
    Func<string, object> d3 = original;

[1]: https://en.wikipedia.org/wiki/Liskov_substitution_principle

## Tipos de variantes como parâmetros e valores de retorno
Se um tipo covariante aparecer como saída, o tipo contido será covariante. Produzir um produtor de `T`s é como produzir `T`s.

    interface IReturnCovariant<out T>
    {
        IEnumerable<T> GetTs();
    }

Se um tipo contravariante aparecer como uma saída, o tipo contido será contravariante. Produzir um consumidor de `T`s é como consumir `T`s.

    interface IReturnContravariant<in T>
    {
        IComparer<T> GetTComparer();
    }

Se um tipo covariante aparecer como entrada, o tipo contido será contravariante. Consumir um produtor de `T`s é como consumir `T`s.

    interface IAcceptCovariant<in T>
    {
        void ProcessTs(IEnumerable<T> ts);
    }

Se um tipo contravariante aparecer como entrada, o tipo contido será covariante. Consumir um consumidor de `T`s é como produzir `T`s.

    interface IAcceptContravariant<out T>
    {
        void CompareTs(IComparer<T> tComparer);
    }

## Parâmetros de tipo (classes)
Declaração:

    class MyGenericClass<T1, T2, T3, ...>
    {
        // Do something with the type parameters.
    }

Inicialização:

    var x = new MyGenericClass<int, char, bool>();

Uso (como o tipo de um parâmetro):

    void AnotherMethod(MyGenericClass<float, byte, char> arg) { ... }

## Parâmetros de tipo (métodos)
Declaração:

    void MyGenericMethod<T1, T2, T3>(T1 a, T2 b, T3 c)
    {
        // Do something with the type parameters.
    }

Invocação:

Não há necessidade de fornecer argumentos de tipo para um método genérico, porque o compilador pode inferir implicitamente o tipo.
    
    int x =10;
    int y =20;
    string z = "test";
    MyGenericMethod(x,y,z);

No entanto, se houver uma ambiguidade, métodos genéricos precisam ser chamados com argumentos de tipo como

    MyGenericMethod<int, int, string>(x,y,z);



## Restrições de tipo (class e struct)
É possível especificar se o argumento de tipo deve ou não ser um tipo de referência ou um tipo de valor usando as respectivas restrições `class` ou `struct`. Se essas restrições forem usadas, elas *deverão* ser definidas _antes que todas as outras restrições (por exemplo, um tipo pai ou `new()`) possam ser listadas.

    // TRef must be a reference type, the use of Int32, Single, etc. is invalid.
    // Interfaces are valid, as they are reference types
    class AcceptsRefType<TRef>
        where TRef : class
    {
        // TStruct must be a value type.
        public void AcceptStruct<TStruct>()
            where TStruct : struct
        {
        }

        // If multiple constraints are used along with class/struct
        // then the class or struct constraint MUST be specified first
        public void Foo<TComparableClass>()
            where TComparableClass : class, IComparable
        {
        }
    }

## Parâmetros de tipo explícito
Existem diferentes casos em que você deve especificar explicitamente os parâmetros de tipo para um método genérico. Em ambos os casos abaixo, o compilador não pode inferir todos os parâmetros de tipo dos parâmetros de método especificados.

Um caso é quando não há parâmetros:

    public void SomeMethod<T, V>() 
    {
       // No code for simplicity
    }

    SomeMethod(); // doesn't compile
    SomeMethod<int, bool>(); // compiles

O segundo caso é quando um (ou mais) dos parâmetros de tipo não faz parte dos parâmetros do método:

    public K SomeMethod<K, V>(V input)
    {
        return default(K);
    }

    int num1 = SomeMethod(3); // doesn't compile
    int num2 = SomeMethod<int>("3"); // doesn't compile
    int num3 = SomeMethod<int, string>("3"); // compiles.

## Cast de tipo genérico
        /// <summary>
        /// Converts a data type to another data type.
        /// </summary>
        public static class Cast
        {
            /// <summary>
            /// Converts input to Type of default value or given as typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (input == null || input == DBNull.Value) return result;
                    if (typeof (T).IsEnum)
                    {
                        result = (T) Enum.ToObject(typeof (T), To(input, Convert.ToInt32(defaultValue)));
                    }
                    else
                    {
                        result = (T) Convert.ChangeType(input, typeof (T));
                    }
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            
            /// <summary>
            /// Converts input to Type of typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input)
            {
                return To(input, default(T));
            }
    
            
    
        }

Usos:

    std.Name = Cast.To<string>(drConnection["Name"]);
    std.Age = Cast.To<int>(drConnection["Age"]);
    std.IsPassed = Cast.To<bool>(drConnection["IsPassed"]);

    
    // Casting type using default value
    //Following both ways are correct
    // Way 1 (In following style input is converted into type of default value)
    std.Name = Cast.To(drConnection["Name"], "");
    std.Marks = Cast.To(drConnection["Marks"], 0);
    // Way 2    
    std.Name = Cast.To<string>(drConnection["Name"], "");
    std.Marks = Cast.To<int>(drConnection["Marks"], 0);

## Leitor de configuração com conversão de tipo genérico
        /// <summary>
        /// Read configuration values from app.config and convert to specified types
        /// </summary>
        public static class ConfigurationReader
        {
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>AppSettings value against key is returned in Type of default value or given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (ConfigurationManager.AppSettings[strKey] != null)
                        result = (T)Convert.ChangeType(ConfigurationManager.AppSettings[strKey], typeof(T));
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <returns>AppSettings value against key is returned in Type given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey)
            {
                return GetConfigKeyValue(strKey, default(T));
            }
    
        }

Usos:

    var timeOut = ConfigurationReader.GetConfigKeyValue("RequestTimeout", 2000);
    var url = ConfigurationReader.GetConfigKeyValue("URL", "www.someurl.com");
    var enabled = ConfigurationReader.GetConfigKeyValue("IsEnabled", false);

