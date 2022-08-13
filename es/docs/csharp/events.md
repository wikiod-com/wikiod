---
title: "Eventos"
slug: "eventos"
draft: false
images: []
weight: 9780
type: docs
toc: true
---

Un evento es una notificación de que algo ha ocurrido (como un clic del mouse) o, en algunos casos, está a punto de ocurrir (como un cambio de precio).

Las clases pueden definir eventos y sus instancias (objetos) pueden generar estos eventos. Por ejemplo, un botón puede contener un evento de clic que se genera cuando un usuario hace clic en él.

Los controladores de eventos son métodos que se llaman cuando se genera su evento correspondiente. Un formulario puede contener un controlador de eventos Clicked para cada botón que contiene, por ejemplo.

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| EventArgsT | El tipo que se deriva de EventArgs y contiene los parámetros del evento. |
| Nombre del evento | El nombre del evento. |
| Nombre del controlador | El nombre del controlador de eventos. |
| SenderObjeto | El objeto que está invocando el evento. |
| Argumentos de evento | Una instancia del tipo EventArgsT que contiene los parámetros del evento.|

Al generar un evento:

* Compruebe siempre si el delegado es `null`. Un delegado nulo significa que el evento no tiene suscriptores. Generar un evento sin suscriptores dará como resultado una `NullReferenceException`.

<!-- si la versión [lt 6.0] -->
* Copie el delegado (p. ej., `EventName`) en una variable local (p. ej., `eventName`) antes de comprobar si es nulo o generar el evento. Esto evita condiciones de carrera en entornos de subprocesos múltiples:

**Equivocado**:

        if(Changed != null)      // Changed has 1 subscriber at this point
                                 // In another thread, that one subscriber decided to unsubscribe
            Changed(this, args); // `Changed` is now null, `NullReferenceException` is thrown.

**Derecha**:

        // Cache the "Changed" event as a local. If it is not null, then use
        // the LOCAL variable (handler) to raise the event, NOT the event itself.
        var handler = Changed;
        if(handler != null)
            handler(this, args);
<!-- versión final si -->

<!-- si la versión [gt 6.0] -->
* Use el operador condicional nulo (?.) para generar el método en lugar de comprobar el valor nulo del delegado para los suscriptores en una instrucción `if`: `EventName?.Invoke(SenderObject, new EventArgsT());`
<!-- versión final si -->

* Al usar Action<> para declarar tipos de delegados, la firma del controlador de eventos/método anónimo debe ser la misma que el tipo de delegado anónimo declarado en la declaración del evento.

## Declarando y Levantando Eventos
## Declaración de un evento

Puedes declarar un evento en cualquier `clase` o `estructura` usando la siguiente sintaxis:

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

Hay una sintaxis ampliada para declarar eventos, donde tiene una instancia privada de
el evento, y define una instancia pública usando los accesores `add` y `set`. La sintaxis es muy similar a las propiedades de C#. En todos los casos, se debe preferir la sintaxis demostrada anteriormente, ya que el compilador emite código para ayudar a garantizar que varios subprocesos puedan agregar y eliminar de forma segura controladores de eventos para el evento en su clase.

## Levantando el Evento

<!-- si la versión [gte 6.0] -->
    
    private void OnMyEvent()
    {
        EventName?.Invoke(this, EventArgs.Empty); 
    }
<!-- versión final si -->
<!-- si la versión [lt 6.0] -->
    
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
<!-- versión final si -->

Tenga en cuenta que los eventos solo pueden ser generados por el tipo de declaración. Los clientes solo pueden suscribirse/darse de baja.

Para las versiones de C# anteriores a la 6.0, donde `EventName?.Invoke` no es compatible, es una buena práctica asignar el evento a una variable temporal antes de la invocación, como se muestra en el ejemplo, lo que garantiza la seguridad de subprocesos en los casos en que se ejecutan varios subprocesos. el mismo código. Si no lo hace, es posible que se genere una `NullReferenceException` en ciertos casos en los que varios subprocesos utilizan la misma instancia de objeto. En C# 6.0, el compilador emite código similar al que se muestra en el ejemplo de código para C# 6.


## Creando un evento cancelable
Una clase puede generar un evento cancelable cuando está a punto de realizar una acción que se puede cancelar, como [`FormClosing`](https://msdn.microsoft.com/en-us/library/system.windows .forms.form.formclosing(v=vs.110).aspx) evento de un [`Form`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form(v =vs.110).aspx).

Para crear tal evento:

- Cree un nuevo argumento de evento derivado de [`CancelEventArgs`](https://msdn.microsoft.com/en-us/library/system.componentmodel.canceleventargs(v=vs.110).aspx) y agregue propiedades adicionales para datos de eventos
- Cree un evento usando `EventHandler<T>` y use la nueva clase de argumento de evento de cancelación que creó.

**Ejemplo**

En el siguiente ejemplo, creamos un evento `PriceChangingEventArgs` para la propiedad `Price` de una clase. La clase de datos de eventos contiene un "Valor" que le permite al consumidor conocer el nuevo. El evento surge cuando asigna un nuevo valor a la propiedad `Precio` y le permite al consumidor saber que el valor está cambiando y le permite cancelar el evento. Si el consumidor cancela el evento, se utilizará el valor anterior para `Precio`:

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

*Producto*

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




## Propiedades del evento
Si una clase genera una gran cantidad de eventos, el costo de almacenamiento de un campo por delegado puede no ser aceptable. .NET Framework proporciona [propiedades de eventos](https://msdn.microsoft.com/en-us/library/8843a9ch(v=vs.110).aspx) para estos casos. De esta forma, puede usar otra estructura de datos como [`EventHandlerList`](https://msdn.microsoft.com/en-us/library/system.componentmodel.eventhandlerlist(v=vs.110).aspx) para almacenar delegados de eventos :

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

Este enfoque se usa ampliamente en marcos de GUI como WinForms, donde los controles pueden tener docenas e incluso cientos de eventos.

Tenga en cuenta que `EventHandlerList` no es seguro para subprocesos, por lo que si espera que su clase se use desde varios subprocesos, deberá agregar instrucciones de bloqueo u otro mecanismo de sincronización (o usar un almacenamiento que proporcione seguridad de subprocesos).

## Declaración de evento estándar
Declaración de evento:
    
    public event EventHandler<EventArgsT> EventName;

Declaración del controlador de eventos:

    public void HandlerName(object sender, EventArgsT args) { /* Handler logic */ }

Suscripción al evento:

*Dinamicamente:*

    EventName += HandlerName;

*A través del Diseñador:*

1. Haga clic en el botón Eventos en la ventana de propiedades del control (Rayo)
2. Haga doble clic en el nombre del evento:

[![ingrese la descripción de la imagen aquí][1]][1]

3. Visual Studio generará el código del evento:


    private void Form1_Load(object sender, EventArgs e)
    {

    }

Invocando el método:
    
    EventName(SenderObject, EventArguments);


[1]: https://i.stack.imgur.com/onqeE.png

## Declaración del controlador de eventos anónimos
Declaración de evento:

    public event EventHandler<EventArgsType> EventName;

Declaración del controlador de eventos usando [operador lambda =>](https://www.wikiod.com/es/docs/c%23/18/operators/12755/lambda-operator#t=20160727203428899399) y suscribiéndose al evento:

    EventName += (obj, eventArgs) => { /* Handler logic */ };

Declaración del controlador de eventos usando la sintaxis del método anónimo [delegate](https://www.wikiod.com/es/docs/c%23/26/keywords/18720/delegate):

    EventName += delegate(object obj, EventArgsType eventArgs) { /* Handler Logic */ };

Declaración y suscripción de un controlador de eventos que no usa el parámetro del evento, por lo que puede usar la sintaxis anterior sin necesidad de especificar parámetros:

    EventName += delegate { /* Handler Logic */ }

Invocando el evento:

    EventName?.Invoke(SenderObject, EventArguments);

## Declaración de eventos no estándar
Los eventos pueden ser de cualquier tipo de delegado, no solo `EventHandler` y `EventHandler<T>`. Por ejemplo:

    //Declaring an event
    public event Action<Param1Type, Param2Type, ...> EventName;

Esto se usa de manera similar a los eventos `EventHandler` estándar:

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

Es posible declarar múltiples eventos del mismo tipo en una sola instrucción, de forma similar a los campos y las variables locales (aunque a menudo esto puede ser una mala idea):

    public event EventHandler Event1, Event2, Event3;

Esto declara tres eventos separados (`Event1`, `Event2` y `Event3`), todos del tipo `EventHandler`.
*Nota: Aunque algunos compiladores pueden aceptar esta sintaxis tanto en las interfaces como en las clases, la especificación de C# (v5.0 §13.2.3) proporciona una gramática para las interfaces que no lo permite, por lo que usar esto en las interfaces puede no ser confiable con diferentes compiladores. .*

## Creación de EventArgs personalizados que contienen datos adicionales
Los eventos personalizados generalmente necesitan argumentos de eventos personalizados que contengan información sobre el evento. Por ejemplo, [`MouseEventArgs`](https://msdn.microsoft.com/en-us/library/system.windows.forms.mouseeventargs(v=vs.110).aspx) que es usado por eventos de mouse como `MouseDown Los eventos ` o `MouseUp` contienen información sobre la `Ubicación` o los `Botones` que solían generar el evento.

Al crear nuevos eventos, para crear un argumento de evento personalizado:

- Cree una clase derivada de [`EventArgs`](https://msdn.microsoft.com/en-us/library/system.eventargs(v=vs.110).aspx) y defina las propiedades para los datos necesarios.
- Por convención, el nombre de la clase debe terminar con `EventArgs`.

**Ejemplo**

En el siguiente ejemplo, creamos un evento `PriceChangingEventArgs` para la propiedad `Price` de una clase. La clase de datos de eventos contiene un `PrecioActual` y un `NuevoPrecio`. El evento surge cuando asigna un nuevo valor a la propiedad 'Precio' y le permite al consumidor saber que el valor está cambiando y le informa sobre el precio actual y el nuevo precio:

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

*Producto*

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

Puede mejorar el ejemplo al permitir que el consumidor cambie el nuevo valor y luego el valor se utilizará para la propiedad. Para ello basta con aplicar estos cambios en las clases.

Cambie la definición de `NewPrice` para que sea configurable:

    public int NewPrice { get; set; }

Cambie la definición de `Price` para usar `e.NewPrice` como valor de propiedad, después de llamar a `OnPriceChanging`:

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




