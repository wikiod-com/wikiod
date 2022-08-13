---
title: "Garbage Collector dans .Net"
slug: "garbage-collector-dans-net"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Références faibles
Dans .NET, le GC alloue des objets lorsqu'il ne leur reste aucune référence. Par conséquent, bien qu'un objet puisse toujours être atteint à partir du code (il y a une référence forte à celui-ci), le GC n'allouera pas cet objet. Cela peut devenir un problème s'il y a beaucoup de gros objets.

Une référence faible est une référence qui permet au GC de collecter l'objet tout en permettant d'accéder à l'objet. Une référence faible n'est valide que pendant la durée indéterminée jusqu'à ce que l'objet soit collecté lorsqu'aucune référence forte n'existe. Lorsque vous utilisez une référence faible, l'application peut toujours obtenir une référence forte à l'objet, ce qui empêche sa collecte. Ainsi, les références faibles peuvent être utiles pour conserver des objets volumineux dont l'initialisation est coûteuse, mais qui doivent être disponibles pour le ramasse-miettes s'ils ne sont pas activement utilisés.

Utilisation simplifiée :

    WeakReference reference = new WeakReference(new object(), false);
    
    GC.Collect();
    
    object target = reference.Target;
    if (target != null)
      DoSomething(target);

Ainsi, des références faibles pourraient être utilisées pour maintenir, par exemple, un cache d'objets. Cependant, il est important de se rappeler qu'il existe toujours un risque que le ramasse-miettes accède à l'objet avant qu'une référence forte ne soit rétablie.

Les références faibles sont également pratiques pour éviter les fuites de mémoire. Un cas d'utilisation typique concerne les événements.

Supposons que nous ayons un gestionnaire d'événement sur une source :

    Source.Event += new EventHandler(Handler)

Ce code enregistre un gestionnaire d'événements et crée une référence forte de la source de l'événement à l'objet d'écoute. Si l'objet source a une durée de vie plus longue que l'écouteur et que l'écouteur n'a plus besoin de l'événement lorsqu'il n'y a plus d'autres références à celui-ci, l'utilisation d'événements .NET normaux provoque une fuite de mémoire : l'objet source contient des objets écouteur en mémoire qui doivent être ramassés.

Dans ce cas, il peut être judicieux d'utiliser le [Weak Event Pattern][1].

Quelque chose comme:

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

et utilisé comme ceci:

     EventSource s = new EventSource();
     Subscriber subscriber = new Subscriber();
     WeakEventManager.SetHandler<Subscriber, SomeEventArgs>(a => s.Event += a, r => s.Event -= r, subscriber, (s,e) => { s.HandleEvent(e); });

Dans ce cas, bien sûr, nous avons certaines restrictions - l'événement doit être un

    public event EventHandler<SomeEventArgs> Event;

Comme le suggère [MSDN][2] :

- N'utilisez de longues références faibles que lorsque cela est nécessaire car l'état du
l'objet est imprévisible après la finalisation.
- Évitez d'utiliser des références faibles à de petits objets car le pointeur
lui-même peut être aussi grand ou plus grand.
- Évitez d'utiliser des références faibles comme solution automatique à la mémoire
problèmes de gestion. Au lieu de cela, développez une stratégie de mise en cache efficace pour
gérer les objets de votre application.


[1] : https://msdn.microsoft.com/en-us/library/aa970850(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/ms404247(v=vs.110).aspx#Anchor_1

## Compactage du tas d'objets volumineux
Par défaut, le tas d'objets volumineux n'est pas compacté contrairement au tas d'objets classique qui [peut entraîner une fragmentation de la mémoire] [1] et, en outre, peut conduire à `OutOfMemoryException`s

À partir de .NET 4.5.1, il existe [une option][2] pour compacter explicitement le tas d'objets volumineux (avec un ramasse-miettes):

    GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce;
    GC.Collect();   

Tout comme toute requête explicite de récupération de place (appelée requête car le CLR n'est pas obligé de l'effectuer), utilisez-la avec précaution et évitez-la par défaut si vous le pouvez, car elle peut décalibrer les statistiques de `GC`, ce qui diminue ses performances.

[1] : https://www.simple-talk.com/dotnet/.net-framework/the-dangers-of-the-large-object-heap/
[2] : https://msdn.microsoft.com/en-us/library/system.runtime.gcsettings.largeobjectheapcompactionmode(v=vs.110).aspx

