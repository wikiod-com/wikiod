---
title: "Implementação Singleton"
slug: "implementacao-singleton"
draft: false
images: []
weight: 9710
type: docs
toc: true
---

## Singleton inicializado estaticamente
    public class Singleton
    {
        private readonly static Singleton instance = new Singleton();
        private Singleton() { }
        public static Singleton Instance => instance;
    }

Esta implementação é thread-safe porque neste caso o objeto `instance` é inicializado no construtor estático. O CLR já garante que todos os construtores estáticos sejam executados com thread-safe.

Mudar `instance` não é uma operação thread-safe, portanto o atributo `readonly` garante a imutabilidade após a inicialização.

## Singleton preguiçoso e seguro para thread (usando Lazy<T>)
O tipo .Net 4.0 Lazy<T> garante a inicialização de objetos thread-safe, portanto, esse tipo pode ser usado para criar Singletons.


    public class LazySingleton
    {
        private static readonly Lazy<LazySingleton> _instance =
            new Lazy<LazySingleton>(() => new LazySingleton());
     
        public static LazySingleton Instance
        {
            get { return _instance.Value; }
        }

        private LazySingleton() { }
    }

Usar `Lazy<T>` garantirá que o objeto seja instanciado apenas quando usado em algum lugar no código de chamada.

Um uso simples será como:

    using System;
                        
    public class Program
    {
        public static void Main()
        {
            var instance = LazySingleton.Instance;
        }
    }

[Demonstração ao vivo no .NET Fiddle][1]

[1]: https://dotnetfiddle.net/oHVpK3

## Singleton preguiçoso e seguro para threads (usando o Double Checked Locking)
Esta versão thread-safe de um singleton era necessária nas primeiras versões do .NET onde a inicialização `static` não era garantida como thread-safe. Em versões mais modernas da estrutura, um [singleton inicializado estaticamente](https://www.wikiod.com/pt/docs/c%23/1192/singleton-implementation/3863/statically-initialized-singleton) é geralmente preferido porque é muito fácil para cometer erros de implementação no padrão a seguir.

    public sealed class ThreadSafeSingleton
    {
       private static volatile ThreadSafeSingleton instance;
       private static object lockObject = new Object();
    
       private ThreadSafeSingleton()
       {
       }
    
       public static ThreadSafeSingleton Instance
       {
          get 
          {
             if (instance == null) 
             {
                lock (lockObject) 
                {
                   if (instance == null)
                   {
                      instance = new ThreadSafeSingleton();
                   }
                }
             }
    
             return instance;
          }
       }
    }

Observe que a verificação `if (instance == null)` é feita duas vezes: uma vez antes do bloqueio ser adquirido e uma vez depois. Essa implementação ainda seria thread-safe mesmo sem a primeira verificação de nulo. No entanto, isso significaria que um bloqueio seria adquirido *toda vez* que a instância fosse solicitada, e isso prejudicaria o desempenho. A primeira verificação nula é adicionada para que o bloqueio não seja adquirido, a menos que seja necessário. A segunda verificação nula garante que apenas o primeiro thread a adquirir o bloqueio crie a instância. Os outros threads encontrarão a instância a ser preenchida e pularão adiante.

## Singleton seguro para threads preguiçoso (para .NET 3.5 ou anterior, implementação alternativa)
Como no .NET 3.5 e anterior você não tem a classe [`Lazy<T>`][1], você usa o seguinte padrão:

    public class Singleton
    {
        private Singleton() // prevents public instantiation
        {
        }
    
        public static Singleton Instance
        {
            get
            {
                return Nested.instance;
            }
        }
        
        private class Nested
        {
            // Explicit static constructor to tell C# compiler
            // not to mark type as beforefieldinit
            static Nested()
            {
            }
    
            internal static readonly Singleton instance = new Singleton();
        }
    }

Isso é inspirado em [post do blog de Jon Skeet][2].

Como a classe `Nested` é aninhada e privada, a instanciação da instância singleton não será acionada ao acessar outros membros da classe `Sigleton` (como uma propriedade somente leitura pública, por exemplo).


[1]: https://msdn.microsoft.com/en-us/library/dd642331(v=vs.110).aspx
[2]: http://www.yoda.arachsys.com/csharp/singleton.html

## Descartando a instância Singleton quando ela não for mais necessária
A maioria dos exemplos mostra a instanciação e retenção de um objeto `LazySingleton` até que o aplicativo proprietário seja encerrado, mesmo que esse objeto não seja mais necessário para o aplicativo. Uma solução para isso é implementar `IDisposable` e definir a instância do objeto como nula da seguinte forma:

    public class LazySingleton : IDisposable
    {
        private static volatile Lazy<LazySingleton> _instance;
        private static volatile int _instanceCount = 0;
        private bool _alreadyDisposed = false;
 
    public static LazySingleton Instance
    {
        get
        {
            if (_instance == null)
                _instance = new Lazy<LazySingleton>(() => new LazySingleton());
            _instanceCount++;
            return _instance.Value;
        }
    }

    private LazySingleton() { }

    // Public implementation of Dispose pattern callable by consumers.
    public void Dispose()
    { 
        if (--_instanceCount == 0) // No more references to this object.
        {       
           Dispose(true);
           GC.SuppressFinalize(this);           
        }
    }
   
    // Protected implementation of Dispose pattern.
    protected virtual void Dispose(bool disposing)
    {
        if (_alreadyDisposed) return; 
      
        if (disposing) 
        {
            _instance = null; // Allow GC to dispose of this instance.
            // Free any other managed objects here.
        }
      
        // Free any unmanaged objects here.
        _alreadyDisposed = true;
    }

O código acima descarta a instância antes do encerramento do aplicativo, mas somente se os consumidores chamarem `Dispose()` no objeto após cada uso. Como não há garantia de que isso acontecerá ou uma maneira de forçá-lo, também não há garantia de que a instância será descartada. Mas se esta classe estiver sendo usada internamente, é mais fácil garantir que o método `Dispose()` seja chamado após cada uso. Segue um exemplo:

    public class Program
    {
        public static void Main()
        {
            using (var instance = LazySingleton.Instance)
            {
                // Do work with instance
            }
        }
    }

Observe que este exemplo **não é seguro para threads**.

