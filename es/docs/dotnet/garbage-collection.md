---
title: "Recolección de basura"
slug: "recoleccion-de-basura"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

En .Net, los objetos creados con new() se asignan en el montón administrado. Estos objetos nunca son finalizados explícitamente por el programa que los usa; en cambio, este proceso está controlado por .Net Garbage Collector.

Algunos de los ejemplos a continuación son "casos de laboratorio" para mostrar el Recolector de Basura en el trabajo y algunos detalles importantes de su comportamiento, mientras que otros se enfocan en cómo preparar las clases para el manejo adecuado por parte del Recolector de Basura.

El Garbage Collector tiene como objetivo reducir el costo del programa en términos de memoria asignada, pero hacerlo tiene un costo en términos de tiempo de procesamiento. Para lograr un buen compromiso general, hay una serie de optimizaciones que deben tenerse en cuenta al programar con el recolector de basura en mente:

- Si el método Collect () se va a invocar explícitamente (lo que no debería ser el caso a menudo), considere usar el modo "optimizado" que finaliza el objeto inactivo solo cuando realmente se necesita memoria
- En lugar de invocar el método Collect(), considere usar los métodos AddMemoryPressure() y RemoveMemoryPressure(), que activan una recopilación de memoria solo si es realmente necesario
- No se garantiza que una colección de memoria finalice todos los objetos inactivos; en cambio, el recolector de basura administra 3 "generaciones", un objeto que a veces "sobrevive" de una generación a la siguiente
- Se pueden aplicar varios modelos de subprocesos, dependiendo de varios factores, incluido el ajuste fino de la configuración, lo que da como resultado diferentes grados de interferencia entre el subproceso del recolector de basura y los otros subprocesos de la aplicación.

  

## Un ejemplo básico de recolección (de basura)
Dada la siguiente clase:

    public class FinalizableObject 
    {
        public FinalizableObject()
        {
            Console.WriteLine("Instance initialized");
        }

        ~FinalizableObject()
        {
            Console.WriteLine("Instance finalized");
        }
    }
Un programa que crea una instancia, incluso sin usarla:

    new FinalizableObject(); // Object instantiated, ready to be used
Produce la siguiente salida:

    <namespace>.FinalizableObject initialized
Si no sucede nada más, el objeto no se finaliza hasta que finaliza el programa (lo que libera todos los objetos en el montón administrado, finalizándolos en el proceso).

Es posible forzar el Garbage Collector para que se ejecute en un punto dado, de la siguiente manera:

    new FinalizableObject(); // Object instantiated, ready to be used
    GC.Collect();
Lo que produce el siguiente resultado:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
Esta vez, tan pronto como se invocó el Recolector de basura, el objeto no utilizado (también conocido como "muerto") se finalizó y se liberó del montón administrado.

## Objetos vivos y objetos muertos: conceptos básicos
Regla general: cuando se produce la recolección de elementos no utilizados, los "objetos vivos" son los que todavía están en uso, mientras que los "objetos muertos" son los que ya no se usan (cualquier variable o campo que haga referencia a ellos, si los hay, ha quedado fuera del alcance antes de que ocurra la recolección) .

En el siguiente ejemplo (por comodidad, FinalizableObject1 y FinalizableObject2 son subclases de FinalizableObject del ejemplo anterior y, por lo tanto, heredan el comportamiento del mensaje de inicialización/finalización):

    var obj1 = new FinalizableObject1(); // Finalizable1 instance allocated here
    var obj2 = new FinalizableObject2(); // Finalizable2 instance allocated here
    obj1 = null; // No more references to the Finalizable1 instance 
    GC.Collect();
La salida será:

    <namespace>.FinalizableObject1 initialized
    <namespace>.FinalizableObject2 initialized
    <namespace>.FinalizableObject1 finalized
En el momento en que se invoca el recolector de elementos no utilizados, FinalizableObject1 es un objeto inactivo y se finaliza, mientras que FinalizableObject2 es un objeto activo y se mantiene en el montón administrado.

## Múltiples objetos muertos
¿Qué pasa si dos (o varios) objetos que de otro modo estarían muertos se referencian entre sí? Esto se muestra en el siguiente ejemplo, suponiendo que OtherObject es una propiedad pública de FinalizableObject:

    var obj1 = new FinalizableObject1(); 
    var obj2 = new FinalizableObject2();
    obj1.OtherObject = obj2;
    obj2.OtherObject = obj1;
    obj1 = null; // Program no longer references Finalizable1 instance
    obj2 = null; // Program no longer references Finalizable2 instance
    // But the two objects still reference each other
    GC.Collect();
Esto produce la siguiente salida:

    <namespace>.FinalizedObject1 initialized
    <namespace>.FinalizedObject2 initialized
    <namespace>.FinalizedObject1 finalized
    <namespace>.FinalizedObject2 finalized
Los dos objetos se finalizan y se liberan del montón administrado a pesar de que se hacen referencia entre sí (porque no existe ninguna otra referencia a ninguno de ellos desde un objeto realmente activo).

## Referencias débiles
Las referencias débiles son... referencias a otros objetos (también conocidos como "objetivos"), pero "débiles" ya que no evitan que esos objetos se recolecten como basura. En otras palabras, las referencias débiles no cuentan cuando el Recolector de basura evalúa los objetos como "vivos" o "muertos".

El siguiente código:

    var weak = new WeakReference<FinalizableObject>(new FinalizableObject());
    GC.Collect();
Produce la salida:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
El objeto se libera del montón administrado a pesar de que la variable WeakReference hace referencia a él (todavía en el ámbito cuando se invocó el recolector de elementos no utilizados).

Consecuencia n.° 1: en cualquier momento, no es seguro asumir si un destino de WeakReference aún está asignado en el montón administrado o no.

Consecuencia #2: cada vez que un programa necesite acceder al objetivo de una Referencia débil, se debe proporcionar el código para ambos casos, si el objetivo aún está asignado o no. El método para acceder al objetivo es TryGetTarget:
 
    var target = new object(); // Any object will do as target
    var weak = new WeakReference<object>(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if(weak.TryGetTarget(out target))
    {
        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }

La versión genérica de WeakReference está disponible desde .Net 4.5. Todas las versiones del marco proporcionan una versión no genérica y sin tipo que se construye de la misma manera y se verifica de la siguiente manera:

    var target = new object(); // Any object will do as target
    var weak = new WeakReference(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if (weak.IsAlive)
    {
        target = weak.Target;

        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }


  

## Dispose() frente a finalizadores
Implemente el método Dispose () (y declare la clase contenedora como IDisposable) como un medio para garantizar que los recursos con gran cantidad de memoria se liberen tan pronto como el objeto ya no se use. El "trampa" es que no hay una fuerte garantía de que el método Dispose() se invoque alguna vez (a diferencia de los finalizadores que siempre se invocan al final de la vida del objeto).

Un escenario es un programa que llama a Dispose() en objetos que crea explícitamente:

    private void SomeFunction()
    {
        // Initialize an object that uses heavy external resources
        var disposableObject = new ClassThatImplementsIDisposable();

        // ... Use that object

        // Dispose as soon as no longer used
        disposableObject.Dispose();

        // ... Do other stuff 

        // The disposableObject variable gets out of scope here
        // The object will be finalized later on (no guarantee when)
        // But it no longer holds to the heavy external resource after it was disposed
    }


Otro escenario es declarar una clase para ser instanciada por el marco. En este caso, la nueva clase normalmente hereda una clase base, por ejemplo, en MVC se crea una clase de controlador como una subclase de System.Web.Mvc.ControllerBase. Cuando la clase base implementa la interfaz IDisposable, esta es una buena pista de que Dispose() sería invocado correctamente por el marco, pero nuevamente, no hay una garantía sólida.

Por lo tanto, Dispose() no sustituye a un finalizador; en cambio, los dos deben usarse para diferentes propósitos:

- Un finalizador eventualmente libera recursos para evitar fugas de memoria que ocurrirían de otra manera
- Dispose() libera recursos (posiblemente los mismos) tan pronto como ya no se necesitan, para aliviar la presión sobre la asignación de memoria general.

## Eliminación y finalización adecuada de los objetos
Como Dispose() y los finalizadores están destinados a diferentes propósitos, una clase que administre recursos de memoria externa pesados ​​​​debería implementar ambos. La consecuencia es escribir la clase para que maneje bien dos escenarios posibles:

- Cuando solo se invoca el finalizador
- Cuando se invoca Dispose() primero y luego también se invoca el finalizador

Una solución es escribir el código de limpieza de tal manera que ejecutarlo una o dos veces produzca el mismo resultado que ejecutarlo solo una vez. La viabilidad depende de la naturaleza de la limpieza, por ejemplo:
- Cerrar una conexión de base de datos ya cerrada probablemente no tendría ningún efecto, por lo que funciona
- Actualizar algún "recuento de uso" es peligroso y produciría un resultado incorrecto si se llama dos veces en lugar de una.

Una solución más segura es garantizar por diseño que el código de limpieza se llame una vez y solo una vez, independientemente del contexto externo. Esto se puede lograr de la "manera clásica" usando una bandera dedicada:

    public class DisposableFinalizable1: IDisposable
    {
        private bool disposed = false;

        ~DisposableFinalizable1() { Cleanup(); }

        public void Dispose() { Cleanup(); }

        private void Cleanup()
        {
            if(!disposed)
            {
                // Actual code to release resources gets here, then
                disposed = true;
            }
        }
    }


Alternativamente, Garbage Collector proporciona un método específico SuppressFinalize() que permite omitir el finalizador después de que se haya invocado Dispose:

    public class DisposableFinalizable2 : IDisposable
    {
        ~DisposableFinalizable2() { Cleanup(); }

        public void Dispose()
        {
            Cleanup();
            GC.SuppressFinalize(this);
        }

        private void Cleanup()
        {
            // Actual code to release resources gets here
        }
    }


 

