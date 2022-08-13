---
title: "Recolector de basura en .Net"
slug: "recolector-de-basura-en-net"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Referencias débiles
En .NET, el GC asigna objetos cuando no quedan referencias a ellos. Por lo tanto, aunque todavía se puede acceder a un objeto desde el código (hay una fuerte referencia a él), el GC no asignará este objeto. Esto puede convertirse en un problema si hay muchos objetos grandes.

Una referencia débil es una referencia que permite que el GC recopile el objeto mientras aún permite acceder al objeto. Una referencia débil es válida solo durante la cantidad de tiempo indeterminada hasta que se recopila el objeto cuando no existen referencias fuertes. Cuando utiliza una referencia débil, la aplicación aún puede obtener una referencia fuerte al objeto, lo que evita que se recopile. Por lo tanto, las referencias débiles pueden ser útiles para retener objetos grandes que son costosos de inicializar, pero que deberían estar disponibles para la recolección de basura si no están en uso activo.

Uso sencillo:

    WeakReference reference = new WeakReference(new object(), false);
    
    GC.Collect();
    
    object target = reference.Target;
    if (target != null)
      DoSomething(target);

Por lo tanto, las referencias débiles podrían usarse para mantener, por ejemplo, un caché de objetos. Sin embargo, es importante recordar que siempre existe el riesgo de que el recolector de basura llegue al objeto antes de que se restablezca una referencia fuerte.

Las referencias débiles también son útiles para evitar pérdidas de memoria. Un caso de uso típico es con eventos.

Supongamos que tenemos algún controlador para un evento en una fuente:

    Source.Event += new EventHandler(Handler)

Este código registra un controlador de eventos y crea una referencia sólida desde el origen del evento hasta el objeto de escucha. Si el objeto de origen tiene una vida útil más larga que el objeto de escucha, y el objeto de escucha ya no necesita el evento cuando no hay otras referencias a él, el uso de eventos .NET normales provoca una pérdida de memoria: el objeto de origen contiene objetos de escucha en la memoria que debe ser basura recogida.

En este caso, puede ser una buena idea usar el [Patrón de evento débil][1].

Algo como:

    public static class WeakEventManager
        {
        public static void SetHandler<S, TArgs>(
        Action<EventHandler<TArgs>> add,
        Action<EventHandler<TArgs>> remove,
        S subscriber,
        Action<S, TArgs> action)
        where TArgs : EventArgs
        where S : class
            {
                var subscrWeakRef = new WeakReference(subscriber);
                EventHandler<TArgs> handler = null;
    
                handler = (s, e) =>
                {
                    var subscrStrongRef = subscrWeakRef.Target as S;
                    if (subscrStrongRef != null)
                    {
                        action(subscrStrongRef, e);
                    }
                    else
                    {
                        remove(handler);
                        handler = null;
                    }
                };
    
                add(handler);
            }
        }

y usado así:

     EventSource s = new EventSource();
     Subscriber subscriber = new Subscriber();
     WeakEventManager.SetHandler<Subscriber, SomeEventArgs>(a => s.Event += a, r => s.Event -= r, subscriber, (s,e) => { s.HandleEvent(e); });

En este caso, por supuesto, tenemos algunas restricciones - el evento debe ser un

    public event EventHandler<SomeEventArgs> Event;

Como sugiere [MSDN][2]:

- Use referencias largas y débiles solo cuando sea necesario como el estado de la
el objeto es impredecible después de la finalización.
- Evite usar referencias débiles a objetos pequeños porque el puntero
sí mismo puede ser tan grande o más grande.
- Evite el uso de referencias débiles como una solución automática a la memoria
problemas de gestión. En su lugar, desarrolle una política de almacenamiento en caché eficaz para
manejar los objetos de su aplicación.


[1]: https://msdn.microsoft.com/en-us/library/aa970850(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms404247(v=vs.110).aspx#Anchor_1

## Compactación de montones de objetos grandes
De forma predeterminada, el Montón de objetos grandes no se compacta a diferencia del Montón de objetos clásico que [puede conducir a la fragmentación de la memoria] [1] y, además, puede provocar `OutOfMemoryException`s

A partir de .NET 4.5.1, existe [una opción][2] para compactar explícitamente el montón de objetos grandes (junto con una recolección de elementos no utilizados):

    GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce;
    GC.Collect();   

Al igual que cualquier solicitud de recolección de basura explícita (se llama solicitud porque el CLR no está obligado a realizarla), utilícela con cuidado y, de forma predeterminada, evítela si puede, ya que puede descalibrar las estadísticas de 'GC', disminuyendo su rendimiento.

[1]: https://www.simple-talk.com/dotnet/.net-framework/the-dangers-of-the-large-object-heap/
[2]: https://msdn.microsoft.com/en-us/library/system.runtime.gcsettings.largeobjectheapcompactionmode(v=vs.110).aspx

