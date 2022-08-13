---
title: "Declaración de bloqueo"
slug: "declaracion-de-bloqueo"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sintaxis
* bloquear (obj) {}

Usando la instrucción `lock` puede controlar el acceso de diferentes subprocesos al código dentro del bloque de código. Se usa comúnmente para evitar condiciones de carrera, por ejemplo, varios subprocesos que leen y eliminan elementos de una colección. Como el bloqueo obliga a los subprocesos a esperar a que otros subprocesos salgan de un bloque de código, puede causar retrasos que podrían resolverse con otros métodos de sincronización.

MSDN

> La palabra clave lock marca un bloque de declaraciones como una sección crítica al
> obtener el bloqueo de exclusión mutua para un objeto dado, ejecutar un
> declaración, y luego liberando el bloqueo.
> 
> La palabra clave lock asegura que un subproceso no entre en un estado crítico
> sección de código mientras otro hilo está en la sección crítica. Si
> otro hilo intenta ingresar un código bloqueado, esperará, bloqueará,
> hasta que se suelte el objeto.
> 
> La mejor práctica es definir un objeto **privado** para bloquear, o un objeto **privado
> variable de objeto estático** para proteger los datos comunes a todas las instancias.

<hr>

En C# 5.0 y versiones posteriores, la sentencia `lock` es equivalente a:

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

Para C# 4.0 y versiones anteriores, la instrucción `lock` es equivalente a:

    System.Threading.Monitor.Enter(refObject);
    try 
    {
        // code
    }
    finally 
    {
         System.Threading.Monitor.Exit(refObject);
    }

## Lanzar una excepción en una declaración de bloqueo
El siguiente código liberará el bloqueo. No habrá problema. La declaración de bloqueo detrás de escena funciona como `intentar finalmente`

    lock(locker)
    {
        throw new Exception();
    }

Se puede ver más en la [Especificación de C# 5.0][1]:

Una sentencia `lock` de la forma

    lock (x) ...

donde `x` es una expresión de un *tipo-referencia*, es precisamente equivalente a
    
    bool __lockWasTaken = false;
    try {
        System.Threading.Monitor.Enter(x, ref __lockWasTaken);
        ...
    }
    finally {
        if (__lockWasTaken) System.Threading.Monitor.Exit(x);
    }

excepto que `x` solo se evalúa una vez.


[1]: https://msdn.microsoft.com/en-us/library/aa664735%28VS.71%29.aspx?f=255&MSPPError=-2147217396

## Uso sencillo
El uso común de `lock` es una sección crítica.

En el siguiente ejemplo, se supone que `ReserveRoom` se llama desde diferentes subprocesos. La sincronización con `lock` es la forma más sencilla de evitar la condición de carrera aquí. El cuerpo del método está rodeado por un `bloqueo` que asegura que dos o más subprocesos no puedan ejecutarlo simultáneamente.
 
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

Si un subproceso alcanza el bloque bloqueado mientras otro subproceso se está ejecutando dentro de él, el primero esperará a otro para salir del bloque.

> La mejor práctica es definir un objeto privado para bloquear, o un objeto privado
> variable de objeto estático para proteger los datos comunes a todas las instancias.

## Regresar en una declaración de bloqueo
El siguiente código liberará el bloqueo.

    lock(locker)
    {
        return 5;
    }

Para una explicación detallada, se recomienda [esta respuesta SO][1].


[1]: http://stackoverflow.com/a/266718/1519458

## Anti-Patrones y trampas
# Bloqueo en una variable local/asignada por la pila

Una de las falacias al usar `lock` es el uso de objetos locales como casillero en una función. Dado que estas instancias de objetos locales diferirán en cada llamada de la función, `lock` no funcionará como se esperaba.

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

# Suponiendo que el bloqueo restringe el acceso al propio objeto de sincronización

Si un subproceso llama: `lock(obj)` y otro subproceso llama `obj.ToString()`, el segundo subproceso no se bloqueará.

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

# Esperando que las subclases sepan cuándo bloquear

A veces, las clases base están diseñadas de tal manera que se requiere que sus subclases usen un bloqueo al acceder a ciertos campos protegidos:

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

Es mucho más seguro *encapsular el bloqueo* usando un [Método de plantilla][3]:

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

# Bloquear una variable ValueType en caja no sincroniza

En el siguiente ejemplo, una variable privada está implícitamente encuadrada, ya que se proporciona como un argumento `objeto` para una función, esperando que se bloquee un recurso de monitor.
El encuadre se produce justo antes de llamar a la función IncInSync, por lo que la instancia encuadrada corresponde a un objeto de montón diferente cada vez que se llama a la función.

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

El boxeo ocurre en la función `Inc`:

    BulemicCounter.Inc:
    IL_0000:  nop         
    IL_0001:  ldarg.0     
    IL_0002:  ldarg.0     
    IL_0003:  ldfld       UserQuery+BulemicCounter.counterLock
    IL_0008:  box         System.Int32**
    IL_000D:  call        UserQuery+BulemicCounter.IncInSync
    IL_0012:  nop         
    IL_0013:  ret         

No significa que un ValueType en caja no se pueda usar para el bloqueo del monitor:

    private readonly object counterLock = 1;

Ahora el boxeo ocurre en el constructor, lo cual está bien para bloquear:

    IL_0001:  ldc.i4.1    
    IL_0002:  box         System.Int32
    IL_0007:  stfld       UserQuery+BulemicCounter.counterLock

# Usar candados innecesariamente cuando existe una alternativa más segura

Un patrón muy común es usar una 'Lista' o 'Diccionario' privado en una clase segura para subprocesos y bloquear cada vez que se accede:

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

Si hay varios métodos que acceden al diccionario de `valores`, el código puede volverse muy largo y, lo que es más importante, bloquear todo el tiempo oscurece su *intención*. El bloqueo también es muy fácil de olvidar y la falta de un bloqueo adecuado puede causar errores muy difíciles de encontrar.

Al usar un [`ConcurrentDictionary`][1], podemos evitar el bloqueo por completo:

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

El uso de colecciones concurrentes también mejora el rendimiento porque [todas ellas emplean técnicas sin bloqueo][2] hasta cierto punto.

[1]: https://msdn.microsoft.com/en-us/library/dd287191%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[2]: https://blogs.msdn.microsoft.com/pfxteam/2010/01/26/faq-are-all-of-the-new-concurrent-collections-lock-free/
[3]: https://en.wikipedia.org/wiki/Template_method_pattern

## Uso de instancias de Object para bloqueo
Cuando se usa la declaración `lock` incorporada de C#, se necesita una instancia de algún tipo, pero su estado no importa. Una instancia de `objeto` es perfecta para esto:

    public class ThreadSafe {
      private static readonly object locker = new object();


      public void SomeThreadSafeMethod() {
        lock (locker) {
          // Only one thread can be here at a time.
        }
      }
    }

**NÓTESE BIEN**. las instancias de `Type` no deben usarse para esto (en el código anterior `typeof(ThreadSafe)`) porque las instancias de `Type` se comparten entre AppDomains y, por lo tanto, se espera que la extensión del bloqueo incluya código que no debería (p. ej. Si `ThreadSafe` se carga en dos AppDomains en el mismo proceso, el bloqueo en su instancia `Type` se bloquearía mutuamente).

