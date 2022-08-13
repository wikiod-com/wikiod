---
title: "Declaração de bloqueio"
slug: "declaracao-de-bloqueio"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sintaxe
* bloqueio (obj) {}

Usando a instrução `lock` você pode controlar o acesso de diferentes threads ao código dentro do bloco de código. É comumente usado para evitar condições de corrida, por exemplo, vários threads lendo e removendo itens de uma coleção. Como o bloqueio força os threads a esperar que outros threads saiam de um bloco de código, isso pode causar atrasos que podem ser resolvidos com outros métodos de sincronização.

MSDN

> A palavra-chave lock marca um bloco de instruções como uma seção crítica por
> obter o bloqueio de exclusão mútua para um determinado objeto, executando um
> e, em seguida, liberando o bloqueio.
> 
> A palavra-chave lock garante que um thread não entre em um
> seção do código enquanto outro segmento está na seção crítica. Se
> outro thread tenta inserir um código bloqueado, ele aguardará, bloqueará,
> até que o objeto seja liberado.
> 
> A prática recomendada é definir um objeto **privado** para bloquear ou um **privado
> variável de objeto estática** para proteger dados comuns a todas as instâncias.

<hr>

No C# 5.0 e posterior, a instrução `lock` é equivalente a:

    bool lockTaken = false;
    try 
    {
        System.Threading.Monitor.Enter(refObject, ref lockTaken);
        // code 
    }
    finally 
    {
        if (lockTaken)
            System.Threading.Monitor.Exit(refObject);
    }

Para C# 4.0 e anteriores, a instrução `lock` é equivalente a:

    System.Threading.Monitor.Enter(refObject);
    try 
    {
        // code
    }
    finally 
    {
         System.Threading.Monitor.Exit(refObject);
    }

## Lançando exceção em uma instrução de bloqueio
O código a seguir liberará o bloqueio. Não haverá problema. Nos bastidores, a instrução de bloqueio funciona como `try finally`

    lock(locker)
    {
        throw new Exception();
    }

Mais informações podem ser vistas na [Especificação C# 5.0][1]:

Uma instrução `lock` do formulário

    lock (x) ...

onde `x` é uma expressão de um *tipo de referência*, é precisamente equivalente a
    
    bool __lockWasTaken = false;
    try {
        System.Threading.Monitor.Enter(x, ref __lockWasTaken);
        ...
    }
    finally {
        if (__lockWasTaken) System.Threading.Monitor.Exit(x);
    }

exceto que `x` é avaliado apenas uma vez.


[1]: https://msdn.microsoft.com/en-us/library/aa664735%28VS.71%29.aspx?f=255&MSPPError=-2147217396

## Uso simples
O uso comum de `lock` é uma seção crítica.

No exemplo a seguir, `ReserveRoom` deve ser chamado de diferentes threads. Sincronização com `lock` é a maneira mais simples de evitar condição de corrida aqui. O corpo do método é cercado por `lock` que garante que duas ou mais threads não possam executá-lo simultaneamente.
 
    public class Hotel
    {
        private readonly object _roomLock = new object();

        public void ReserveRoom(int roomNumber)
        {
            // lock keyword ensures that only one thread executes critical section at once
            // in this case, reserves a hotel room of given number
            // preventing double bookings
            lock (_roomLock)
            {
                // reserve room logic goes here
            }
        }
    }

Se uma thread alcançar o bloco `lock`-ed enquanto outra thread estiver rodando dentro dele, a primeira esperará outra para sair do bloco.

> A melhor prática é definir um objeto privado para bloquear ou um
> variável de objeto estático para proteger dados comuns a todas as instâncias.

## Retorna em uma instrução de bloqueio
O código a seguir liberará o bloqueio.

    lock(locker)
    {
        return 5;
    }

Para uma explicação detalhada, [esta resposta do SO][1] é recomendado.


[1]: http://stackoverflow.com/a/266718/1519458

## Anti-padrões e pegadinhas
# Bloqueando em uma variável local / alocada pela pilha

Uma das falácias ao usar `lock` é o uso de objetos locais como locker em uma função. Como essas instâncias de objetos locais serão diferentes em cada chamada da função, `lock` não funcionará como esperado.

    List<string> stringList = new List<string>();

    public void AddToListNotThreadSafe(string something)
    {
        // DO NOT do this, as each call to this method 
        // will lock on a different instance of an Object.
        // This provides no thread safety, it only degrades performance.
        var localLock = new Object();
        lock(localLock)
        {
            stringList.Add(something);
        }
    }

    // Define object that can be used for thread safety in the AddToList method
    readonly object classLock = new object();

    public void AddToList(List<string> stringList, string something)
    {
        // USE THE classLock instance field to achieve a 
        // thread-safe lock before adding to stringList
        lock(classLock)
        {
            stringList.Add(something);
        }
    }

# Assumindo que o bloqueio restringe o acesso ao próprio objeto de sincronização

Se uma thread chamar: `lock(obj)` e outra thread chamar `obj.ToString()` a segunda thread não será bloqueada.

    object obj = new Object();
     
    public void SomeMethod()
    {
         lock(obj)
        {
           //do dangerous stuff 
        }
     }

     //Meanwhile on other tread 
     public void SomeOtherMethod()
     {
       var objInString = obj.ToString(); //this does not block
     }

# Esperando que as subclasses saibam quando bloquear

Às vezes, as classes base são projetadas de forma que suas subclasses sejam obrigadas a usar um bloqueio ao acessar certos campos protegidos:

    public abstract class Base
    {
        protected readonly object padlock;
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public abstract void Do();
    }

    public class Derived1 : Base
    {
        public override void Do()
        {
            lock (this.padlock)
            {
                this.list.Add("Derived1");
            }
        }
    }

    public class Derived2 : Base
    {
        public override void Do()
        {
            this.list.Add("Derived2"); // OOPS! I forgot to lock!
        }
    }

É muito mais seguro *encapsular o bloqueio* usando um [Método de Modelo][3]:

    public abstract class Base
    {
        private readonly object padlock; // This is now private
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public void Do()
        {
            lock (this.padlock) {
                this.DoInternal();
            }
        }

        protected abstract void DoInternal();
    }

    public class Derived1 : Base
    {
        protected override void DoInternal()
        {
            this.list.Add("Derived1"); // Yay! No need to lock
        }
    }

# Bloquear em uma variável ValueType em caixa não sincroniza

No exemplo a seguir, uma variável privada é implicitamente encaixotada, pois é fornecida como um argumento `object` para uma função, esperando que um recurso de monitor seja bloqueado.
O boxing ocorre logo antes de chamar a função IncInSync, portanto, a instância em caixa corresponde a um objeto de heap diferente cada vez que a função é chamada.

    public int Count { get; private set; }

    private readonly int counterLock = 1;
    
    public void Inc()
    {
        IncInSync(counterLock);
    }

    private void IncInSync(object monitorResource)
    {
        lock (monitorResource)
        {
            Count++;
        }
    }

O boxe ocorre na função `Inc`:

    BulemicCounter.Inc:
    IL_0000:  nop         
    IL_0001:  ldarg.0     
    IL_0002:  ldarg.0     
    IL_0003:  ldfld       UserQuery+BulemicCounter.counterLock
    IL_0008:  box         System.Int32**
    IL_000D:  call        UserQuery+BulemicCounter.IncInSync
    IL_0012:  nop         
    IL_0013:  ret         

Isso não significa que um ValueType em caixa não possa ser usado para bloqueio de monitor:

    private readonly object counterLock = 1;

Agora o boxing ocorre no construtor, o que é bom para travar:

    IL_0001:  ldc.i4.1    
    IL_0002:  box         System.Int32
    IL_0007:  stfld       UserQuery+BulemicCounter.counterLock

# Usando travas desnecessariamente quando existe uma alternativa mais segura

Um padrão muito comum é usar uma `List` ou `Dictionary` privada em uma classe thread-safe e travar toda vez que ela for acessada:

    public class Cache
    {
        private readonly object padlock;
        private readonly Dictionary<string, object> values;

        public WordStats()
        {
            this.padlock = new object();
            this.values = new Dictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            lock (this.padlock)
            {
                this.values.Add(key, value);
            }
        }

        /* rest of class omitted */
    }

Se houver vários métodos acessando o dicionário `values`, o código pode ficar muito longo e, mais importante, travar o tempo todo obscurece sua *intenção*. O bloqueio também é muito fácil de esquecer e a falta de bloqueio adequado pode causar erros muito difíceis de encontrar.

Usando um [`ConcurrentDictionary`][1], podemos evitar o bloqueio completo:

    public class Cache
    {
        private readonly ConcurrentDictionary<string, object> values;

        public WordStats()
        {
            this.values = new ConcurrentDictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            this.values.Add(key, value);
        }

        /* rest of class omitted */
    }

O uso de coleções simultâneas também melhora o desempenho porque [todos eles empregam técnicas sem bloqueio] [2] até certo ponto.

[1]: https://msdn.microsoft.com/en-us/library/dd287191%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[2]: https://blogs.msdn.microsoft.com/pfxteam/2010/01/26/faq-are-all-of-the-new-concurrent-collections-lock-free/
[3]: https://en.wikipedia.org/wiki/Template_method_pattern

## Usando instâncias de Object para bloqueio
Ao usar a instrução `lock` embutida do C#, uma instância de algum tipo é necessária, mas seu estado não importa. Uma instância de `object` é perfeita para isso:

    public class ThreadSafe {
      private static readonly object locker = new object();


      public void SomeThreadSafeMethod() {
        lock (locker) {
          // Only one thread can be here at a time.
        }
      }
    }

**NO**. instâncias de `Type` não devem ser usadas para isso (no código acima de `typeof(ThreadSafe)`) porque instâncias de `Type` são compartilhadas entre AppDomains e, portanto, a extensão do bloqueio pode incluir código que não deveria (por exemplo, . se `ThreadSafe` for carregado em dois AppDomains no mesmo processo, o bloqueio em sua instância `Type` será bloqueado mutuamente).

