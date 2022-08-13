---
title: "Coletor de lixo em .Net"
slug: "coletor-de-lixo-em-net"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Referências fracas
Em .NET, o GC aloca objetos quando não há referências a eles. Portanto, enquanto um objeto ainda pode ser alcançado a partir do código (há uma forte referência a ele), o GC não alocará esse objeto. Isso pode se tornar um problema se houver muitos objetos grandes.

Uma referência fraca é uma referência que permite ao GC coletar o objeto enquanto ainda permite acessar o objeto. Uma referência fraca é válida apenas durante o período de tempo indeterminado até que o objeto seja coletado quando não existirem referências fortes. Quando você usa uma referência fraca, o aplicativo ainda pode obter uma referência forte ao objeto, o que impede que ele seja coletado. Portanto, referências fracas podem ser úteis para manter objetos grandes que são caros para inicializar, mas devem estar disponíveis para coleta de lixo se não estiverem em uso ativamente.

Uso simples:

    WeakReference reference = new WeakReference(new object(), false);
    
    GC.Collect();
    
    object target = reference.Target;
    if (target != null)
      DoSomething(target);

Assim, referências fracas podem ser usadas para manter, por exemplo, um cache de objetos. No entanto, é importante lembrar que sempre existe o risco de o coletor de lixo chegar ao objeto antes que uma referência forte seja restabelecida.

Referências fracas também são úteis para evitar vazamentos de memória. Um caso de uso típico é com eventos.

Suponha que tenhamos algum manipulador para um evento em uma fonte:

    Source.Event += new EventHandler(Handler)

Esse código registra um manipulador de eventos e cria uma referência forte da fonte do evento para o objeto de escuta. Se o objeto de origem tiver um tempo de vida mais longo do que o ouvinte, e o ouvinte não precisar mais do evento quando não houver outras referências a ele, o uso de eventos .NET normais causará um vazamento de memória: o objeto de origem retém objetos ouvintes na memória que deve ser coletado no lixo.

Neste caso, pode ser uma boa ideia usar o [Padrão de Evento Fraco][1].

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

e usado assim:

     EventSource s = new EventSource();
     Subscriber subscriber = new Subscriber();
     WeakEventManager.SetHandler<Subscriber, SomeEventArgs>(a => s.Event += a, r => s.Event -= r, subscriber, (s,e) => { s.HandleEvent(e); });

Neste caso, claro, temos algumas restrições - o evento deve ser um

    public event EventHandler<SomeEventArgs> Event;

Como [MSDN][2] sugere:

- Use referências longas e fracas apenas quando necessário, pois o estado do
objeto é imprevisível após a finalização.
- Evite usar referências fracas para objetos pequenos porque o ponteiro
em si pode ser tão grande ou maior.
- Evite usar referências fracas como uma solução automática para a memória
problemas de gestão. Em vez disso, desenvolva uma política de cache eficaz para
manipulando os objetos do seu aplicativo.


[1]: https://msdn.microsoft.com/en-us/library/aa970850(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms404247(v=vs.110).aspx#Anchor_1

## Compactação de heap de objetos grandes
Por padrão, o Large Object Heap não é compactado ao contrário do Object Heap clássico que [pode levar à fragmentação da memória][1] e, além disso, pode levar a `OutOfMemoryException`s

A partir do .NET 4.5.1, há [uma opção][2] para compactar explicitamente o heap de objetos grandes (junto com uma coleta de lixo):

    GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce;
    GC.Collect();   

Assim como qualquer requisição de coleta de lixo explícita (chama-se requisição porque o CLR não é obrigado a conduzi-la) use com cuidado e por padrão evite-a se puder, pois ela pode descalibrar as estatísticas do `GC`, diminuindo seu desempenho.

[1]: https://www.simple-talk.com/dotnet/.net-framework/the-dangers-of-the-large-object-heap/
[2]: https://msdn.microsoft.com/en-us/library/system.runtime.gcsettings.largeobjectheapcompactionmode(v=vs.110).aspx

