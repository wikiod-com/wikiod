---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9780
type: docs
toc: true
---

An event is a notification that something has occurred (such as a mouse click) or, in some cases, is about to occur (such as a price change).

Classes can define events and their instances (objects) may raise these events. For instance, a Button may contain a Click event that gets raised when a user has clicked it.

Event handlers are then methods that get called when their corresponding event is raised. A form may contain a Clicked event handler for every Button it contains, for instance.

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| EventArgsT | The type that derives from EventArgs and contains the event parameters. |  
| EventName | The name of the event. |
| HandlerName | The name of the event handler. |
| SenderObject | The object that's invoking the event. |
| EventArguments | An instance of the EventArgsT type that contains the event parameters.|

When raising an event:

* Always check if the delegate is `null`. A null delegate means the event has no subscribers. Raising an event with no subscribers will result in a `NullReferenceException`.

<!-- if version [lt 6.0] -->
* Copy the delegate (e.g. `EventName`) to a local variable (e.g. `eventName`) before checking for null / raising the event. This avoids race conditions in multi-threaded environments:

**Wrong**:

        if(Changed != null)      // Changed has 1 subscriber at this point
                                 // In another thread, that one subscriber decided to unsubscribe
            Changed(this, args); // `Changed` is now null, `NullReferenceException` is thrown.

**Right**:

        // Cache the "Changed" event as a local. If it is not null, then use
        // the LOCAL variable (handler) to raise the event, NOT the event itself.
        var handler = Changed;
        if(handler != null)
            handler(this, args);
<!-- end version if -->

<!-- if version [gt 6.0] -->
* Use the null-conditional operator (?.) for raising the method instead of null-checking the delegate for subscribers in an `if` statement: `EventName?.Invoke(SenderObject, new EventArgsT());`
<!-- end version if -->

* When using Action<> to declare delegate types, the anonymous method / event handler signature must be the same as the declared anonymous delegate type in the event declaration.

## Declaring and Raising Events
## Declaring an Event

You can declare an event on any `class` or `struct` using the following syntax:

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

There is an expanded syntax for declaring events, where you hold a private instance of
the event, and define a public instance using `add` and `set` accessors. The syntax is very similar to C# properties. In all cases, the syntax demonstrated above should be preferred, because the compiler emits code to help ensure that multiple threads can safely add and remove event handlers to the event on your class.

## Raising the Event

<!-- if version [gte 6.0] -->
    
    private void OnMyEvent()
    {
        EventName?.Invoke(this, EventArgs.Empty); 
    }
<!-- end version if -->    
<!-- if version [lt 6.0] -->
    
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
<!-- end version if -->  

Note that events can only be raised by the declaring type. Clients can only subscribe/unsubscribe.

For C# versions before 6.0, where `EventName?.Invoke` is not supported, it is a good practice to assign the event to a temporary variable before invocation, as shown in the example, which ensures thread-safety in cases where multiple threads execute the same code. Failing to do so may cause a `NullReferenceException` to be thrown in certain cases where multiple threads are using the same object instance. In C# 6.0, the compiler emits code similar to that shown in the code example for C# 6.


## Creating cancelable event
A cancelable event can be raised by a class when it is about to perform an action that can be canceled, such as the [`FormClosing`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form.formclosing(v=vs.110).aspx)  event of a [`Form`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form(v=vs.110).aspx). 

To create such event:

 - Create a new event arg deriving from [`CancelEventArgs`](https://msdn.microsoft.com/en-us/library/system.componentmodel.canceleventargs(v=vs.110).aspx) and add additional properties for event data.
 - Create an event using `EventHandler<T>` and use the new cancel event arg class which you created.

**Example**

In the below example, we create a `PriceChangingEventArgs` event for `Price` property of a class. The event data class contains a `Value` which let the consumer know about the new . The event raises when you assign a new value to `Price` property and lets the consumer know the value is changing and let them to cancel the event. If the consumer cancels the event, the previous value for `Price` will be used:

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

*Product*

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




## Event Properties
If a class raises a large the number of events, the storage cost of one field per delegate may not be acceptable. The .NET Framework provides [event properties](https://msdn.microsoft.com/en-us/library/8843a9ch(v=vs.110).aspx) for these cases. This way you can use another data structure like [`EventHandlerList`](https://msdn.microsoft.com/en-us/library/system.componentmodel.eventhandlerlist(v=vs.110).aspx) to store event delegates:

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

This approach is widely used in GUI frameworks like WinForms where controls can have dozens and even hundreds of events.

Note that `EventHandlerList` is not thread-safe, so if you expect your class to be used from multiple threads, you will need to add lock statements or other synchronization mechanism (or use a storage that provides thread safety).

## Standard Event Declaration
Event declaration:
    
    public event EventHandler<EventArgsT> EventName;

Event handler declaration:

    public void HandlerName(object sender, EventArgsT args) { /* Handler logic */ }

Subscribing to the event:

*Dynamically:*

    EventName += HandlerName;

*Through the Designer:*

 1. Click the Events button on the control's properties window (Lightening bolt)
 2. Double-click the Event name:

[![enter image description here][1]][1]

 3. Visual Studio will generate the event code:


    private void Form1_Load(object sender, EventArgs e)
    {

    }

Invoking the method:
    
    EventName(SenderObject, EventArguments);


  [1]: https://i.stack.imgur.com/onqeE.png

## Anonymous Event Handler Declaration
Event declaration:

    public event EventHandler<EventArgsType> EventName;

Event handler declaration using [lambda operator =>](https://www.wikiod.com/docs/c%23/18/operators/12755/lambda-operator#t=20160727203428899399) and subscribing to the event:

    EventName += (obj, eventArgs) => { /* Handler logic */ };

Event handler declaration using [delegate](https://www.wikiod.com/docs/c%23/26/keywords/18720/delegate) anonymous method syntax:

    EventName += delegate(object obj, EventArgsType eventArgs) { /* Handler Logic */ };

Declaration & subscription of an event handler that does not use the event's parameter, and so can use the above syntax without needing to specify parameters:

    EventName += delegate { /* Handler Logic */ }

Invoking the event:

    EventName?.Invoke(SenderObject, EventArguments);

## Non-Standard Event Declaration
Events can be of any delegate type, not just `EventHandler` and `EventHandler<T>`. For example:

    //Declaring an event
    public event Action<Param1Type, Param2Type, ...> EventName;

This is used similarly to standard `EventHandler` events:

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

It is possible to declare multiple events of the same type in a single statement, similar to with fields and local variables (though this may often be a bad idea):

    public event EventHandler Event1, Event2, Event3;

This declares three separate events (`Event1`, `Event2`, and `Event3`) all of type `EventHandler`.  
*Note: Although some compilers may accept this syntax in interfaces as well as classes, the C# specification (v5.0 §13.2.3) provides grammar for interfaces that does not allow it, so using this in interfaces may be unreliable with different compilers.*

## Creating custom EventArgs containing additional data
Custom events usually need custom event arguments containing information about the event. For example [`MouseEventArgs`](https://msdn.microsoft.com/en-us/library/system.windows.forms.mouseeventargs(v=vs.110).aspx) which is used by mouse events like `MouseDown` or `MouseUp` events, contains information about `Location` or `Buttons` which used to generate the event.

When creating new events, to create a custom event arg:

 - Create a class deriving from [`EventArgs`](https://msdn.microsoft.com/en-us/library/system.eventargs(v=vs.110).aspx) and define properties for necessary data.
 - As a convention, the name of the class should ends with `EventArgs`.

**Example**

In the below example, we create a `PriceChangingEventArgs` event for `Price` property of a class. The event data class contains a `CurrentPrice` and a `NewPrice`. The event raises when you assign a new value to `Price` property and lets the consumer know the value is changing and let them to know about current price and new price:

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

*Product*

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

You can enhance the example by allowing the consumer to change the new value and then the value will be used for property. To do so it's enough to apply these changes in classes.

Change the definition of `NewPrice` to be settable: 

    public int NewPrice { get; set; }

Change the definition of `Price` to use `e.NewPrice` as value of property, after calling `OnPriceChanging`  :

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




