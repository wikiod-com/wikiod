---
title: "Eventos"
slug: "eventos"
draft: false
images: []
weight: 9780
type: docs
toc: true
---

Um evento é uma notificação de que algo ocorreu (como um clique do mouse) ou, em alguns casos, está prestes a ocorrer (como uma alteração de preço).

As classes podem definir eventos e suas instâncias (objetos) podem gerar esses eventos. Por exemplo, um Button pode conter um evento Click que é gerado quando um usuário clica nele.

Manipuladores de eventos são métodos que são chamados quando seu evento correspondente é gerado. Um formulário pode conter um manipulador de eventos Clicked para cada Button que ele contém, por exemplo.

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| EventArgsT | O tipo que deriva de EventArgs e contém os parâmetros do evento. |
| NomedoEvento | O nome do evento. |
| HandlerName | O nome do manipulador de eventos. |
| SenderObject | O objeto que está invocando o evento. |
| EventArguments | Uma instância do tipo EventArgsT que contém os parâmetros do evento.|

Ao gerar um evento:

* Sempre verifique se o delegado é `null`. Um delegado nulo significa que o evento não tem assinantes. Gerar um evento sem assinantes resultará em um `NullReferenceException`.

<!-- if versão [lt 6.0] -->
* Copie o delegado (por exemplo, `EventName`) para uma variável local (por exemplo, `eventName`) antes de verificar se há nulo/gerar o evento. Isso evita condições de corrida em ambientes multithread:

**Errado**:

        if(Changed != null)      // Changed has 1 subscriber at this point
                                 // In another thread, that one subscriber decided to unsubscribe
            Changed(this, args); // `Changed` is now null, `NullReferenceException` is thrown.

**Certo**:

        // Cache the "Changed" event as a local. If it is not null, then use
        // the LOCAL variable (handler) to raise the event, NOT the event itself.
        var handler = Changed;
        if(handler != null)
            handler(this, args);
<!-- versão final if -->

<!-- if versão [gt 6.0] -->
* Use o operador condicional nulo (?.) para gerar o método em vez de verificar o delegado para assinantes em uma instrução `if`: `EventName?.Invoke(SenderObject, new EventArgsT());`
<!-- versão final if -->

* Ao usar Action<> para declarar tipos de delegado, o método anônimo/assinatura do manipulador de eventos deve ser igual ao tipo de delegado anônimo declarado na declaração do evento.

## Declarando e Levantando Eventos
## Declarando um evento

Você pode declarar um evento em qualquer `class` ou `struct` usando a seguinte sintaxe:

    public class MyClass
    {
        // Declares the event for MyClass
        public event EventHandler MyEvent;

        // Raises the MyEvent event
        public void RaiseEvent()
        {
            OnMyEvent();
        }
    }    

Há uma sintaxe expandida para declarar eventos, onde você mantém uma instância privada de
o evento e defina uma instância pública usando os acessadores `add` e `set`. A sintaxe é muito semelhante às propriedades do C#. Em todos os casos, a sintaxe demonstrada acima deve ser preferida, porque o compilador emite código para ajudar a garantir que vários threads possam adicionar e remover manipuladores de eventos com segurança para o evento em sua classe.

## Aumentando o evento

<!-- if versão [gte 6.0] -->
    
    private void OnMyEvent()
    {
        EventName?.Invoke(this, EventArgs.Empty); 
    }
<!-- versão final if -->
<!-- if versão [lt 6.0] -->
    
    private void OnMyEvent()
    {
        // Use a local for EventName, because another thread can modify the
        // public EventName between when we check it for null, and when we
        // raise the event.
        var eventName = EventName;

        // If eventName == null, then it means there are no event-subscribers,
        // and therefore, we cannot raise the event.
        if(eventName != null)
            eventName(this, EventArgs.Empty);
    
    }
<!-- versão final if -->

Observe que os eventos só podem ser gerados pelo tipo de declaração. Os clientes só podem se inscrever/cancelar a inscrição.

Para versões C# anteriores a 6.0, onde `EventName?.Invoke` não é suportado, é uma boa prática atribuir o evento a uma variável temporária antes da invocação, conforme mostrado no exemplo, que garante a segurança de thread nos casos em que vários threads são executados o mesmo código. Deixar de fazer isso pode fazer com que uma `NullReferenceException` seja lançada em certos casos em que vários threads estão usando a mesma instância de objeto. No C# 6.0, o compilador emite código semelhante ao mostrado no exemplo de código do C# 6.


## Criando evento cancelável
Um evento cancelável pode ser gerado por uma classe quando está prestes a executar uma ação que pode ser cancelada, como o [`FormClosing`](https://msdn.microsoft.com/en-us/library/system.windows .forms.form.formclosing(v=vs.110).aspx) evento de um [`Form`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form(v =vs.110).aspx).

Para criar tal evento:

- Crie um novo argumento de evento derivado de [`CancelEventArgs`](https://msdn.microsoft.com/en-us/library/system.componentmodel.canceleventargs(v=vs.110).aspx) e adicione propriedades adicionais para dados do evento.
- Crie um evento usando `EventHandler<T>` e use a nova classe cancel event arg que você criou.

**Exemplo**

No exemplo abaixo, criamos um evento `PriceChangingEventArgs` para a propriedade `Price` de uma classe. A classe de dados do evento contém um `Value` que informa ao consumidor sobre o novo arquivo . O evento é gerado quando você atribui um novo valor à propriedade `Price` e informa ao consumidor que o valor está mudando e permite que ele cancele o evento. Se o consumidor cancelar o evento, será utilizado o valor anterior de `Price`:

*PriceChangingEventArgs*

    public class PriceChangingEventArgs : CancelEventArgs
    {
        int value;
        public int Value
        {
            get { return value; }
        }
        public PriceChangingEventArgs(int value)
        {
            this.value = value;
        }
    }

*Produtos*

    public class Product
    {
        int price;
        public int Price
        {
            get { return price; }
            set
            {
                var e = new PriceChangingEventArgs(value);
                OnPriceChanging(e);
                if (!e.Cancel)
                    price = value;
            }
        }

        public event EventHandler<PriceChangingEventArgs> PropertyChanging;
        protected void OnPriceChanging(PriceChangingEventArgs e)
        {
            var handler = PropertyChanging;
            if (handler != null)
                PropertyChanging(this, e);
        }
    }




## Propriedades do evento
Se uma classe gera um grande número de eventos, o custo de armazenamento de um campo por delegado pode não ser aceitável. O .NET Framework fornece [propriedades do evento](https://msdn.microsoft.com/en-us/library/8843a9ch(v=vs.110).aspx) para esses casos. Dessa forma, você pode usar outra estrutura de dados como [`EventHandlerList`](https://msdn.microsoft.com/en-us/library/system.componentmodel.eventhandlerlist(v=vs.110).aspx) para armazenar delegados de eventos :

    public class SampleClass 
    {
        // Define the delegate collection.
        protected EventHandlerList eventDelegates = new EventHandlerList();

        // Define a unique key for each event.
        static readonly object someEventKey = new object();
 
        // Define the SomeEvent event property.
        public event EventHandler SomeEvent
        {
            add
            {
                // Add the input delegate to the collection.
                eventDelegates.AddHandler(someEventKey, value);
            }
            remove
            {
                // Remove the input delegate from the collection.
                eventDelegates.RemoveHandler(someEventKey, value);
            }
        }

        // Raise the event with the delegate specified by someEventKey
        protected void OnSomeEvent(EventArgs e)
        {
            var handler = (EventHandler)eventDelegates[someEventKey];
            if (handler != null)
                handler(this, e);
        }
    }

Essa abordagem é amplamente usada em estruturas de GUI como WinForms, onde os controles podem ter dezenas e até centenas de eventos.

Observe que `EventHandlerList` não é thread-safe, portanto, se você espera que sua classe seja usada em vários threads, você precisará adicionar instruções de bloqueio ou outro mecanismo de sincronização (ou usar um armazenamento que forneça segurança de thread).

## Declaração de evento padrão
Declaração do evento:
    
    public event EventHandler<EventArgsT> EventName;

Declaração do manipulador de eventos:

    public void HandlerName(object sender, EventArgsT args) { /* Handler logic */ }

Inscrevendo-se no evento:

*Dinamicamente:*

    EventName += HandlerName;

*Através do Designer:*

1. Clique no botão Eventos na janela de propriedades do controle (Lightening bolt)
2. Clique duas vezes no nome do evento:

[![digite a descrição da imagem aqui][1]][1]

3. O Visual Studio gerará o código do evento:


    private void Form1_Load(object sender, EventArgs e)
    {

    }

Invocando o método:
    
    EventName(SenderObject, EventArguments);


[1]: https://i.stack.imgur.com/onqeE.png

## Declaração de manipulador de eventos anônimos
Declaração do evento:

    public event EventHandler<EventArgsType> EventName;

Declaração do manipulador de eventos usando [operador lambda =>](https://www.wikiod.com/pt/docs/c%23/18/operators/12755/lambda-operator#t=20160727203428899399) e assinando o evento:

    EventName += (obj, eventArgs) => { /* Handler logic */ };

Declaração do manipulador de eventos usando a sintaxe do método anônimo [delegate](https://www.wikiod.com/pt/docs/c%23/26/keywords/18720/delegate):

    EventName += delegate(object obj, EventArgsType eventArgs) { /* Handler Logic */ };

Declaração e assinatura de um manipulador de eventos que não usa o parâmetro do evento e, portanto, pode usar a sintaxe acima sem precisar especificar parâmetros:

    EventName += delegate { /* Handler Logic */ }

Invocando o evento:

    EventName?.Invoke(SenderObject, EventArguments);

## Declaração de evento não padrão
Os eventos podem ser de qualquer tipo de delegado, não apenas `EventHandler` e `EventHandler<T>`. Por exemplo:

    //Declaring an event
    public event Action<Param1Type, Param2Type, ...> EventName;

Isso é usado de forma semelhante aos eventos `EventHandler` padrão:

    //Adding a named event handler
    public void HandlerName(Param1Type parameter1, Param2Type parameter2, ...) {
        /* Handler logic */
    }
    EventName += HandlerName;

    //Adding an anonymous event handler
    EventName += (parameter1, parameter2, ...) => { /* Handler Logic */ };

    //Invoking the event
    EventName(parameter1, parameter2, ...);

---

É possível declarar vários eventos do mesmo tipo em uma única instrução, semelhante a campos e variáveis ​​locais (embora isso possa ser uma má ideia):

    public event EventHandler Event1, Event2, Event3;

Isso declara três eventos separados (`Event1`, `Event2` e `Event3`) todos do tipo `EventHandler`.
*Observação: Embora alguns compiladores possam aceitar essa sintaxe em interfaces, bem como em classes, a especificação C# (v5.0 §13.2.3) fornece gramática para interfaces que não a permitem, portanto, usar isso em interfaces pode não ser confiável com compiladores diferentes .*

## Criando EventArgs personalizados contendo dados adicionais
Os eventos personalizados geralmente precisam de argumentos de eventos personalizados contendo informações sobre o evento. Por exemplo [`MouseEventArgs`](https://msdn.microsoft.com/en-us/library/system.windows.forms.mouseeventargs(v=vs.110).aspx) que é usado por eventos de mouse como `MouseDown ` ou `MouseUp`, contém informações sobre `Location` ou `Buttons` que costumavam gerar o evento.

Ao criar novos eventos, para criar um argumento de evento personalizado:

- Crie uma classe derivada de [`EventArgs`](https://msdn.microsoft.com/en-us/library/system.eventargs(v=vs.110).aspx) e defina propriedades para os dados necessários.
- Por convenção, o nome da classe deve terminar com `EventArgs`.

**Exemplo**

No exemplo abaixo, criamos um evento `PriceChangingEventArgs` para a propriedade `Price` de uma classe. A classe de dados do evento contém um `CurrentPrice` e um `NewPrice`. O evento é gerado quando você atribui um novo valor à propriedade `Price` e informa ao consumidor que o valor está mudando e informa sobre o preço atual e o novo preço:

*PriceChangingEventArgs*

    public class PriceChangingEventArgs : EventArgs
    {
        public PriceChangingEventArgs(int currentPrice, int newPrice)
        {
            this.CurrentPrice = currentPrice;
            this.NewPrice = newPrice;
        }

        public int CurrentPrice { get; private set; }
        public int NewPrice { get; private set; }
    }

*Produtos*

    public class Product
    {
        public event EventHandler<PriceChangingEventArgs> PriceChanging;

        int price;
        public int Price
        {
            get { return price; }
            set
            {
                var e = new PriceChangingEventArgs(price, value);
                OnPriceChanging(e);
                price = value;
            }
        }

        protected void OnPriceChanging(PriceChangingEventArgs e)
        {
            var handler = PriceChanging;
            if (handler != null)
                handler(this, e);
        }
    }

Você pode aprimorar o exemplo permitindo que o consumidor altere o novo valor e, em seguida, o valor será usado para propriedade. Para isso basta aplicar essas alterações nas classes.

Altere a definição de `NewPrice` para ser configurável:

    public int NewPrice { get; set; }

Altere a definição de `Price` para usar `e.NewPrice` como valor da propriedade, após chamar `OnPriceChanging`:

    int price;
    public int Price
    {
        get { return price; }
        set
        {
            var e = new PriceChangingEventArgs(price, value);
            OnPriceChanging(e);
            price = e.NewPrice;
        }
    }




