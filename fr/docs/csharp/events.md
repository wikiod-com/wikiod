---
title: "Événements"
slug: "evenements"
draft: false
images: []
weight: 9780
type: docs
toc: true
---

Un événement est une notification indiquant que quelque chose s'est produit (comme un clic de souris) ou, dans certains cas, est sur le point de se produire (comme un changement de prix).

Les classes peuvent définir des événements et leurs instances (objets) peuvent déclencher ces événements. Par exemple, un Button peut contenir un événement Click qui est déclenché lorsqu'un utilisateur a cliqué dessus.

Les gestionnaires d'événements sont alors des méthodes qui sont appelées lorsque leur événement correspondant est déclenché. Un formulaire peut contenir un gestionnaire d'événements Clicked pour chaque bouton qu'il contient, par exemple.

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| EventArgsT | Le type qui dérive de EventArgs et contient les paramètres d'événement. |
| NomÉvénement | Le nom de l'évènement. |
| Nom du gestionnaire | Nom du gestionnaire d'événements. |
| SenderObject | L'objet qui appelle l'événement. |
| ÉvénementArguments | Une instance du type EventArgsT qui contient les paramètres d'événement.|

Lors de la création d'un événement :

* Vérifiez toujours si le délégué est `null`. Un délégué nul signifie que l'événement n'a pas d'abonnés. Le déclenchement d'un événement sans abonné entraînera une `NullReferenceException`.

<!-- si version [lt 6.0] -->
* Copiez le délégué (par exemple, `EventName`) dans une variable locale (par exemple, `eventName`) avant de vérifier la valeur null / de déclencher l'événement. Cela évite les conditions de concurrence dans les environnements multithread :

**Mauvais**:

        if(Changed != null)      // Changed has 1 subscriber at this point
                                 // In another thread, that one subscriber decided to unsubscribe
            Changed(this, args); // `Changed` is now null, `NullReferenceException` is thrown.

**Droit**:

        // Cache the "Changed" event as a local. If it is not null, then use
        // the LOCAL variable (handler) to raise the event, NOT the event itself.
        var handler = Changed;
        if(handler != null)
            handler(this, args);
<!-- fin de version si -->

<!-- si version [gt 6.0] -->
* Utilisez l'opérateur conditionnel nul (?.) pour élever la méthode au lieu de vérifier la valeur nulle du délégué pour les abonnés dans une instruction `if` : `EventName?.Invoke(SenderObject, new EventArgsT());`
<!-- fin de version si -->

* Lorsque vous utilisez Action<> pour déclarer des types de délégués, la méthode anonyme/la signature du gestionnaire d'événements doit être identique au type de délégué anonyme déclaré dans la déclaration d'événement.

## Déclarer et déclencher des événements
## Déclarer un événement

Vous pouvez déclarer un événement sur n'importe quelle `class` ou `struct` en utilisant la syntaxe suivante :

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

Il existe une syntaxe étendue pour déclarer des événements, où vous détenez une instance privée de
l'événement et définissez une instance publique à l'aide des accesseurs `add` et `set`. La syntaxe est très similaire aux propriétés C#. Dans tous les cas, la syntaxe illustrée ci-dessus doit être préférée, car le compilateur émet du code pour garantir que plusieurs threads peuvent ajouter et supprimer en toute sécurité des gestionnaires d'événements à l'événement de votre classe.

## Élever l'événement

<!-- si version [gte 6.0] -->
    
    private void OnMyEvent()
    {
        EventName?.Invoke(this, EventArgs.Empty); 
    }
<!-- fin de version si -->
<!-- si version [lt 6.0] -->
    
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
<!-- fin de version si -->

Notez que les événements ne peuvent être déclenchés que par le type déclarant. Les clients peuvent uniquement s'abonner/se désabonner.

Pour les versions C# antérieures à 6.0, où `EventName?.Invoke` n'est pas pris en charge, il est recommandé d'affecter l'événement à une variable temporaire avant l'invocation, comme indiqué dans l'exemple, ce qui garantit la sécurité des threads dans les cas où plusieurs threads s'exécutent. le même code. Ne pas le faire peut entraîner la levée d'une `NullReferenceException` dans certains cas où plusieurs threads utilisent la même instance d'objet. Dans C# 6.0, le compilateur émet un code similaire à celui présenté dans l'exemple de code pour C# 6.


## Création d'un événement annulable
Un événement annulable peut être déclenché par une classe lorsqu'elle est sur le point d'effectuer une action pouvant être annulée, telle que [`FormClosing`](https://msdn.microsoft.com/en-us/library/system.windows .forms.form.formclosing(v=vs.110).aspx) événement d'un [`Form`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form(v =vs.110).aspx).

Pour créer un tel événement :

- Créez un nouvel argument d'événement dérivé de [`CancelEventArgs`](https://msdn.microsoft.com/en-us/library/system.componentmodel.canceleventargs(v=vs.110).aspx) et ajoutez des propriétés supplémentaires pour données d'événement.
- Créez un événement à l'aide de `EventHandler<T>` et utilisez la nouvelle classe d'arg d'événement d'annulation que vous avez créée.

**Exemple**

Dans l'exemple ci-dessous, nous créons un événement `PriceChangingEventArgs` pour la propriété `Price` d'une classe. La classe de données d'événement contient une `Value` qui informe le consommateur du nouveau fichier . L'événement se déclenche lorsque vous affectez une nouvelle valeur à la propriété `Price` et informe le consommateur que la valeur change et lui permet d'annuler l'événement. Si le consommateur annule l'événement, la valeur précédente de `Price` sera utilisée :

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

*Produit*

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




## Propriétés de l'événement
Si une classe déclenche un grand nombre d'événements, le coût de stockage d'un champ par délégué peut ne pas être acceptable. Le .NET Framework fournit [propriétés d'événement](https://msdn.microsoft.com/en-us/library/8843a9ch(v=vs.110).aspx) pour ces cas. De cette façon, vous pouvez utiliser une autre structure de données comme [`EventHandlerList`](https://msdn.microsoft.com/en-us/library/system.componentmodel.eventhandlerlist(v=vs.110).aspx) pour stocker les délégués d'événements :

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

Cette approche est largement utilisée dans les frameworks GUI comme WinForms où les contrôles peuvent avoir des dizaines voire des centaines d'événements.

Notez que `EventHandlerList` n'est pas thread-safe, donc si vous vous attendez à ce que votre classe soit utilisée à partir de plusieurs threads, vous devrez ajouter des instructions de verrouillage ou un autre mécanisme de synchronisation (ou utiliser un stockage qui assure la sécurité des threads).

## Déclaration d'événement standard
Déclaration d'événement :
    
    public event EventHandler<EventArgsT> EventName;

Déclaration du gestionnaire d'événements :

    public void HandlerName(object sender, EventArgsT args) { /* Handler logic */ }

Inscription à l'événement :

*Dynamiquement :*

    EventName += HandlerName;

*Par l'intermédiaire du concepteur :*

1. Cliquez sur le bouton Evénements de la fenêtre des propriétés du champ (Eclair)
2. Double-cliquez sur le nom de l'événement :

[![entrez la description de l'image ici][1]][1]

3. Visual Studio générera le code de l'événement :


    private void Form1_Load(object sender, EventArgs e)
    {

    }

Appel de la méthode :
    
    EventName(SenderObject, EventArguments);


[1] : https://i.stack.imgur.com/onqeE.png

## Déclaration du gestionnaire d'événements anonyme
Déclaration d'événement :

    public event EventHandler<EventArgsType> EventName;

Déclaration du gestionnaire d'événements à l'aide de [lambda operator =>](https://www.wikiod.com/fr/docs/c%23/18/operators/12755/lambda-operator#t=20160727203428899399) et en s'abonnant à l'événement :

    EventName += (obj, eventArgs) => { /* Handler logic */ };

Déclaration du gestionnaire d'événements à l'aide de la syntaxe de la méthode anonyme [delegate](https://www.wikiod.com/fr/docs/c%23/26/keywords/18720/delegate) :

    EventName += delegate(object obj, EventArgsType eventArgs) { /* Handler Logic */ };

Déclaration et abonnement d'un gestionnaire d'événements qui n'utilise pas le paramètre de l'événement, et peut donc utiliser la syntaxe ci-dessus sans avoir besoin de spécifier de paramètres :

    EventName += delegate { /* Handler Logic */ }

Invoquer l'événement :

    EventName?.Invoke(SenderObject, EventArguments);

## Déclaration d'événement non standard
Les événements peuvent être de n'importe quel type de délégué, pas seulement `EventHandler` et `EventHandler<T>`. Par exemple:

    //Declaring an event
    public event Action<Param1Type, Param2Type, ...> EventName;

Ceci est utilisé de la même manière que les événements `EventHandler` standard :

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

Il est possible de déclarer plusieurs événements du même type dans une seule instruction, comme avec des champs et des variables locales (bien que cela puisse souvent être une mauvaise idée) :

    public event EventHandler Event1, Event2, Event3;

Cela déclare trois événements distincts (`Event1`, `Event2` et `Event3`) tous de type `EventHandler`.
*Remarque : Bien que certains compilateurs acceptent cette syntaxe dans les interfaces ainsi que dans les classes, la spécification C# (v5.0 §13.2.3) fournit une grammaire pour les interfaces qui ne l'autorisent pas. Par conséquent, son utilisation dans les interfaces peut ne pas être fiable avec différents compilateurs. .*

## Création d'EventArgs personnalisés contenant des données supplémentaires
Les événements personnalisés nécessitent généralement des arguments d'événement personnalisés contenant des informations sur l'événement. Par exemple [`MouseEventArgs`](https://msdn.microsoft.com/en-us/library/system.windows.forms.mouseeventargs(v=vs.110).aspx) qui est utilisé par les événements de souris comme `MouseDown ` ou `MouseUp`, contient des informations sur `Location` ou `Buttons` utilisés pour générer l'événement.

Lors de la création de nouveaux événements, pour créer un argument d'événement personnalisé :

- Créez une classe dérivée de [`EventArgs`](https://msdn.microsoft.com/en-us/library/system.eventargs(v=vs.110).aspx) et définissez les propriétés des données nécessaires.
- Par convention, le nom de la classe doit se terminer par `EventArgs`.

**Exemple**

Dans l'exemple ci-dessous, nous créons un événement `PriceChangingEventArgs` pour la propriété `Price` d'une classe. La classe de données d'événement contient un `CurrentPrice` et un `NewPrice`. L'événement se déclenche lorsque vous attribuez une nouvelle valeur à la propriété `Price` et informe le consommateur que la valeur change et lui fait connaître le prix actuel et le nouveau prix :

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

*Produit*

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

Vous pouvez améliorer l'exemple en permettant au consommateur de modifier la nouvelle valeur, puis la valeur sera utilisée pour la propriété. Pour ce faire, il suffit d'appliquer ces changements dans les classes.

Modifiez la définition de `NewPrice` pour qu'elle soit paramétrable :

    public int NewPrice { get; set; }

Modifiez la définition de `Price` pour utiliser `e.NewPrice` comme valeur de propriété, après avoir appelé `OnPriceChanging` :

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




