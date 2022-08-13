---
title: "Construtores e Finalizadores"
slug: "construtores-e-finalizadores"
draft: false
images: []
weight: 9573
type: docs
toc: true
---

Construtores são métodos em uma classe que são invocados quando uma instância dessa classe é criada. Sua principal responsabilidade é deixar o novo objeto em um estado útil e consistente.

Destrutores/finalizadores são métodos em uma classe que são invocados quando uma instância dela é destruída. Em C# eles raramente são escritos/usados ​​explicitamente.

C# na verdade não tem destruidores, mas sim Finalizadores que usam a sintaxe do destruidor de estilo C++. Especificar um destruidor substitui o método `Object.Finalize()` que não pode ser chamado diretamente.

Ao contrário de outras linguagens com sintaxe semelhante, esses métodos *não* são chamados quando os objetos saem do escopo, mas são chamados quando o Garbage Collector é executado, o que ocorre [sob certas condições][3]. Como tal, eles *não* são garantidos para serem executados em qualquer ordem específica.

Os finalizadores devem ser responsáveis ​​por limpar os recursos não gerenciados **somente** (ponteiros adquiridos por meio da classe Marshal, recebidos por meio de p/Invoke (chamadas de sistema) ou ponteiros brutos usados ​​em blocos não seguros). Para limpar os recursos gerenciados, revise IDisposable, o padrão Dispose e a instrução [`using`][1].

(Leitura adicional: [Quando devo criar um destruidor?][2])


[1]: https://www.wikiod.com/pt/docs/c%23/38/using-statement
[2]: http://stackoverflow.com/a/4899622
[3]: https://msdn.microsoft.com/en-us/library/ee787088(v=vs.110).aspx#conditions_for_a_garbage_collection

## Construtor estático
Um construtor estático é chamado na primeira vez que qualquer membro de um tipo é inicializado, um membro de classe estático é chamado ou um método estático.
O construtor estático é thread-safe.
Um construtor estático é comumente usado para:
- Inicialize o estado estático, que é o estado compartilhado entre diferentes instâncias da mesma classe.
- Criar um singleton

**Exemplo:**

    class Animal
    {
        // * A static constructor is executed only once,
        //   when a class is first accessed.
        // * A static constructor cannot have any access modifiers
        // * A static constructor cannot have any parameters
        static Animal()
        {
            Console.WriteLine("Animal initialized");
        }

        // Instance constructor, this is executed every time the class is created
        public Animal()
        {
            Console.WriteLine("Animal created");
        }

        public static void Yawn()
        {
            Console.WriteLine("Yawn!");
        }
    }

    var turtle = new Animal();
    var giraffe = new Animal();
**Resultado:**
 
> Animal inicializado
> Animal criado
> Animal criado

[Ver demonstração][1]

Se a primeira chamada for para um método estático, o construtor estático será invocado sem o construtor de instância. Isso está correto, porque o método estático não pode acessar o estado da instância de qualquer maneira.

    Animal.Yawn();

Isso irá gerar:

> Animal inicializado
> Bocejo!

Consulte também [Exceções em construtores estáticos][2] e [Construtores estáticos genéricos][3] .


[1]: https://dotnetfiddle.net/XmExII
[2]: https://www.wikiod.com/pt/docs/c%23/25/constructors-finalizers/15007/exceptions-in-static-constructors
[3]: https://www.wikiod.com/pt/docs/c%23/25/constructors-finalizers/15003/generic-static-constructors

Exemplo de singleton:

    public class SessionManager
    {
        public static SessionManager Instance;

        static SessionManager()
        {
            Instance = new SessionManager();
        }
    }

## Padrão construtor singleton
    public class SingletonClass
    {
        public static SingletonClass Instance { get; } = new SingletonClass();

        private SingletonClass()
        {
            // Put custom constructor code here
        }    
    }

Como o construtor é privado, nenhuma nova instância de `SingletonClass` pode ser feita consumindo código. A única maneira de acessar a única instância de `SingletonClass` é usando a propriedade estática `SingletonClass.Instance`.

A propriedade `Instance` é atribuída por um construtor estático que o compilador C# gera. O tempo de execução .NET garante que o construtor estático seja executado no máximo uma vez e seja executado antes da primeira leitura de 'Instance'. Portanto, todas as preocupações de sincronização e inicialização são realizadas pelo tempo de execução.

Observe que, se o construtor estático falhar, a classe `Singleton` se tornará permanentemente inutilizável durante a vida útil do AppDomain.

Além disso, não é garantido que o construtor estático seja executado no momento do primeiro acesso de `Instance`. Em vez disso, ele será executado *em algum momento antes disso*. Isso torna o momento em que a inicialização ocorre não determinístico. Em casos práticos, o JIT geralmente chama o construtor estático durante a *compilação* (não a execução) de um método referenciando `Instance`. Esta é uma otimização de desempenho.

Consulte a página [Implementações Singleton][1] para outras formas de implementar o padrão singleton.


[1]: https://www.wikiod.com/pt/docs/c%23/1192/singleton-implementation#t=201607231143190778053

## Construtor padrão
Quando um tipo é definido sem um construtor:

    public class Animal
    {
    }

então o compilador gera um construtor padrão equivalente ao seguinte:

    public class Animal
    {
        public Animal() {}
    }

A definição de qualquer construtor para o tipo suprimirá a geração do construtor padrão. Se o tipo foi definido da seguinte forma:

    public class Animal
    {
        public Animal(string name) {}
    }

então um `Animal` só poderia ser criado chamando o construtor declarado.

    // This is valid
    var myAnimal = new Animal("Fluffy");
    // This fails to compile
    var unnamedAnimal = new Animal();

Para o segundo exemplo, o compilador exibirá uma mensagem de erro:
>'Animal' não contém um construtor que recebe 0 argumentos

Se você quiser que uma classe tenha um construtor sem parâmetros e um construtor que receba um parâmetro, você pode fazer isso implementando explicitamente ambos os construtores.

    public class Animal
    {
        
        public Animal() {} //Equivalent to a default constructor.
        public Animal(string name) {}
    }

O compilador não poderá gerar um construtor padrão se a classe estender outra classe que não tenha um construtor sem parâmetros. Por exemplo, se tivéssemos uma classe `Creature`:

    public class Creature
    {
        public Creature(Genus genus) {}
    }

então `Animal` definido como `class Animal : Creature {}` não compilaria.

## Forçando um construtor estático a ser chamado
Enquanto construtores estáticos são sempre chamados antes do primeiro uso de um tipo, às vezes é útil poder forçá-los a serem chamados e a classe `RuntimeHelpers` fornece um auxiliar para isso:

    using System.Runtime.CompilerServices;    
    // ...
    RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);

***Observação*:** Todas as inicializações estáticas (inicializadores de campos, por exemplo) serão executadas, não apenas o construtor em si.

***Potenciais usos*:** Forçar a inicialização durante a tela inicial em um aplicativo de interface do usuário ou garantir que um construtor estático não falhe em um teste de unidade.

## Chamando um construtor de outro construtor
    public class Animal
    {
        public string Name { get; set; }

        public Animal() : this("Dog")
        {
        }

        public Animal(string name)
        {
            Name = name;
        }
    }

    var dog = new Animal();      // dog.Name will be set to "Dog" by default.
    var cat = new Animal("Cat"); // cat.Name is "Cat", the empty constructor is not called.


## Chamando o construtor da classe base
Um construtor de uma classe base é chamado antes que um construtor de uma classe derivada seja executado. Por exemplo, se `Mammal` estende `Animal`, então o código contido no construtor de `Animal` é chamado primeiro ao criar uma instância de um `Mammal`.

Se uma classe derivada não especificar explicitamente qual construtor da classe base deve ser chamado, o compilador assume o construtor sem parâmetros.

    public class Animal
    {
        public Animal() { Console.WriteLine("An unknown animal gets born."); }
        public Animal(string name) { Console.WriteLine(name + " gets born"); }
    }

    public class Mammal : Animal
    {
        public Mammal(string name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

Neste caso, instanciar um `Mammal` chamando `new Mammal("George the Cat")` imprimirá

>Nasce um animal desconhecido.
>George the Cat é um mamífero.

[Ver demonstração][1]

Chamar um construtor diferente da classe base é feito colocando `: base(args)` entre a assinatura do construtor e seu corpo:

    public class Mammal : Animal
    {
        public Mammal(string name) : base(name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

Chamar `new Mammal("George the Cat")` agora imprimirá:

>George the Cat nasce.
>George the Cat é um mamífero.

[Ver demonstração][2]


[1]: https://dotnetfiddle.net/xb8Vqr
[2]: https://dotnetfiddle.net/gbdERq

## Finalizadores em classes derivadas
Quando um gráfico de objeto é finalizado, a ordem é inversa da construção. Por exemplo. o supertipo é finalizado antes do tipo base como o código a seguir demonstra:

    class TheBaseClass
    {
        ~TheBaseClass() 
        {
            Console.WriteLine("Base class finalized!");
        }
    }
    
    class TheDerivedClass : TheBaseClass
    {
        ~TheDerivedClass() 
        {
            Console.WriteLine("Derived class finalized!");
        }
    }

    //Don't assign to a variable
    //to make the object unreachable
    new TheDerivedClass();
    
    //Just to make the example work;
    //this is otherwise NOT recommended!
    GC.Collect();

    //Derived class finalized!
    //Base class finalized!

## Exceções em construtores estáticos
Se um construtor estático lançar uma exceção, ela nunca será repetida. O tipo é inutilizável durante a vida útil do AppDomain. Quaisquer outros usos do tipo irão gerar um `TypeInitializationException` envolvendo a exceção original.


    public class Animal
    {
        static Animal()
        {
            Console.WriteLine("Static ctor");
            throw new Exception();
        }
    
        public static void Yawn() {}
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

Isso irá gerar:

> ctor estático
>
> System.TypeInitializationException: O inicializador de tipo
> para 'Animal' lançou uma exceção. ---> System.Exception: Exceção de
> tipo 'System.Exception' foi lançado.

[...]

> System.TypeInitializationException: O inicializador de tipo para 'Animal'
> lançou uma exceção. ---> System.Exception: Exceção do tipo
> 'System.Exception' foi lançado.

onde você pode ver que o construtor real é executado apenas uma vez e a exceção é reutilizada.

## Chamando métodos virtuais no construtor
Ao contrário do C++ em C#, você pode chamar um método virtual do construtor de classe (OK, você também pode em C++, mas o comportamento a princípio é surpreendente). Por exemplo:

    abstract class Base
    {
        protected Base()
        {
            _obj = CreateAnother();
        }
    
        protected virtual AnotherBase CreateAnother()
        {
            return new AnotherBase();
        }
    
        private readonly AnotherBase _obj;
    }
    
    sealed class Derived : Base
    {
        public Derived() { }
    
        protected override AnotherBase CreateAnother()
        {
            return new AnotherDerived();
        }
    }
    
    var test = new Derived();
    // test._obj is AnotherDerived

Se você vem de um background em C++, isso é surpreendente, o construtor da classe base já vê a tabela de métodos virtuais da classe derivada!

**Cuidado**: a classe derivada pode não ter sido totalmente inicializada ainda (seu construtor será executado após o construtor da classe base) e esta técnica é perigosa (há também um aviso StyleCop para isso). Normalmente, isso é considerado uma má prática.


## Construtores estáticos genéricos
Se o tipo no qual o construtor estático é declarado for genérico, o construtor estático será chamado uma vez para cada combinação exclusiva de argumentos genéricos.

    class Animal<T>
    {
        static Animal()
        {
            Console.WriteLine(typeof(T).FullName);
        }

        public static void Yawn() { }
    }

    Animal<Object>.Yawn();
    Animal<String>.Yawn();

Isso irá gerar:

> System.Object
> System.String

Veja também [Como funcionam os construtores estáticos para tipos genéricos?][1]

[1]: http://stackoverflow.com/q/5629388

## Inicialização do Construtor e da Propriedade
A atribuição do valor da propriedade deve ser executada *antes* ou *depois* do construtor da classe?

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    var testInstance = new TestClass() { TestProperty = 1 };

No exemplo acima, o valor de `TestProperty` deve ser `1` no construtor da classe ou após o construtor da classe?

----

Atribuindo valores de propriedade na criação da instância assim:

    var testInstance = new TestClass() {TestProperty = 1};

Será executado ***após*** o construtor ser executado. No entanto, inicializando o valor da propriedade na propriedade da classe no C# 6.0 assim:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;

        public TestClass() 
        {
        }
    }

será feito ***antes*** do construtor ser executado.

---

Combinando os dois conceitos acima em um único exemplo:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    static void Main(string[] args) 
    {
        var testInstance = new TestClass() { TestProperty = 1 };
        Console.WriteLine(testInstance.TestProperty); //resulting in 1
    }

Resultado final:

    "Or shall this be executed"
    "1"

---

**Explicação:**

O valor `TestProperty` será atribuído primeiro como `2`, então o construtor `TestClass` será executado, resultando na impressão de

    "Or shall this be executed"
    
E então o `TestProperty` será atribuído como `1` devido a `new TestClass() { TestProperty = 1 }`, fazendo com que o valor final para o `TestProperty` impresso por `Console.WriteLine(testInstance.TestProperty)` seja

    "1"



