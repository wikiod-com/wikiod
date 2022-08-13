---
title: "Implementación Singleton"
slug: "implementacion-singleton"
draft: false
images: []
weight: 9710
type: docs
toc: true
---

## Singleton inicializado estáticamente
    public class Singleton
    {
        private readonly static Singleton instance = new Singleton();
        private Singleton() { }
        public static Singleton Instance => instance;
    }

Esta implementación es segura para subprocesos porque en este caso el objeto `instancia` se inicializa en el constructor estático. El CLR ya garantiza que todos los constructores estáticos se ejecuten de forma segura para subprocesos.

Mutar `instance` no es una operación segura para subprocesos, por lo tanto, el atributo `readonly` garantiza la inmutabilidad después de la inicialización.

## Singleton perezoso y seguro para subprocesos (usando Lazy<T>)
.Net 4.0 type Lazy<T> garantiza la inicialización de objetos segura para subprocesos, por lo que este tipo podría usarse para crear Singletons.


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

El uso de `Lazy<T>` se asegurará de que el objeto solo se instancia cuando se usa en algún lugar del código de llamada.

Un uso simple será como:

    using System;
                        
    public class Program
    {
        public static void Main()
        {
            var instance = LazySingleton.Instance;
        }
    }

[Demostración en vivo en .NET Fiddle][1]

[1]: https://dotnetfiddle.net/oHVpK3

## Singleton perezoso y seguro para subprocesos (con bloqueo de doble verificación)
Esta versión segura para subprocesos de un singleton era necesaria en las primeras versiones de .NET, donde no se garantizaba que la inicialización "estática" fuera segura para subprocesos. En versiones más modernas del marco, generalmente se prefiere un [singleton inicializado estáticamente] (https://www.wikiod.com/es/docs/c%23/1192/singleton-implementation/3863/statically-initialized-singleton) porque es muy fácil cometer errores de implementación en el siguiente patrón.

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

Tenga en cuenta que la verificación `if (instancia == nulo)` se realiza dos veces: una antes de adquirir el bloqueo y otra después. Esta implementación seguiría siendo segura para subprocesos incluso sin la primera verificación nula. Sin embargo, eso significaría que se adquiriría un bloqueo *cada vez* que se solicita la instancia, y eso afectaría el rendimiento. Se agrega el primer cheque nulo para que no se adquiera el bloqueo a menos que sea necesario. La segunda verificación nula se asegura de que solo el primer subproceso para adquirir el bloqueo cree la instancia. Los otros subprocesos encontrarán la instancia que se completará y saltarán adelante.

## Singleton perezoso y seguro para subprocesos (para .NET 3.5 o anterior, implementación alternativa)
Debido a que en .NET 3.5 y anteriores no tiene la clase [`Lazy<T>`][1], usa el siguiente patrón:

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

Esto está inspirado en [la entrada del blog de Jon Skeet][2].

Debido a que la clase `Nested` está anidada y es privada, la instanciación de la instancia singleton no se activará al acceder a otros miembros de la clase `Sigleton` (como una propiedad pública de solo lectura, por ejemplo).


[1]: https://msdn.microsoft.com/en-us/library/dd642331(v=vs.110).aspx
[2]: http://www.yoda.arachsys.com/csharp/singleton.html

## Eliminación de la instancia de Singleton cuando ya no se necesita
La mayoría de los ejemplos muestran la creación de instancias y la retención de un objeto `LazySingleton` hasta que la aplicación propietaria haya terminado, incluso si la aplicación ya no necesita ese objeto. Una solución a esto es implementar `IDisposable` y establecer la instancia del objeto en nulo de la siguiente manera:

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

El código anterior elimina la instancia antes de la finalización de la aplicación, pero solo si los consumidores llaman a `Dispose()` en el objeto después de cada uso. Dado que no hay garantía de que esto suceda o una forma de forzarlo, tampoco hay garantía de que la instancia se elimine alguna vez. Pero si esta clase se usa internamente, es más fácil asegurarse de que el método `Dispose()` se llame después de cada uso. Un ejemplo sigue:

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

Tenga en cuenta que este ejemplo **no es seguro para subprocesos**.

