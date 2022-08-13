---
title: "Observer"
slug: "observer"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

[![enter image description here][1]][1]

[1]: http://i.imgur.com/jP1ujds.png
What is the intent ?
- Adopt the principle of separation of concerns.
- Create a separation between the subject and the observer.
- Allow multiple observers to react to change a single subject.

What is the structure ?
- Subject provides a way to Register, Unregister, Notify.
- Observer provides a way to update.

## Observer / Java
The observer pattern lets users of a class subscribe to events that happen when this class processes data etc. and be notified when these event occur. In the following example we create a processing class and an observer class which will be notified while processing a phrase - if it finds words that are longer than 5 letters.

The `LongWordsObserver` interface defines the observer. Implement this interface in order to register an observer to events.

<!-- language: java -->
    // an observe that can be registered and receive notifications
    public interface LongWordsObserver {
        void notify(WordEvent event);
    }
    
The `WordEvent` class is the event that will be sent to the observer classes once certain events occur (in this case, long words were found)
<!-- language: java -->
    // An event class which contains the long word that was found
    public class WordEvent {
    
        private String word;
    
        public WordEvent(String word) {
            this.word = word;
        }
    
        public String getWord() {
            return word;
        }
    }
    
The `PhraseProcessor` class is the class that processes the given phrase. It allows observers to be registered using the `addObserver` method. Once long words are found, these observers will be called using an instance of the `WordEvent` class.
<!-- language: java -->
    import java.util.ArrayList;
    import java.util.List;
    
    public class PhraseProcessor {
    
        // the list of observers
        private List<LongWordsObserver> observers = new ArrayList<>();
    
        // register an observer
        public void addObserver(LongWordsObserver observer) {
            observers.add(observer);
        }
    
        // inform all the observers that a long word was found
        private void informObservers(String word) {
            observers.forEach(o -> o.notify(new WordEvent(word)));
        }
    
        // the main method - process a phrase and look for long words. If such are found,
        // notify all the observers
        public void process(String phrase) {
            for (String word : phrase.split(" ")) {
                if (word.length() > 5) {
                    informObservers(word);
                }
            }
        }
    }

The `LongWordsExample` class shows how to register observers, call the `process` method and receive alerts when long words were found.
<!-- language: java -->
    import java.util.ArrayList;
    import java.util.List;
    
    public class LongWordsExample {

        public static void main(String[] args) {
    
            // create a list of words to be filled when long words were found
            List<String> longWords = new ArrayList<>();
    
            // create the PhraseProcessor class
            PhraseProcessor processor = new PhraseProcessor();

            // register an observer and specify what it should do when it receives events,
            // namely to append long words in the longwords list
            processor.addObserver(event -> longWords.add(event.getWord()));
    
            // call the process method 
            processor.process("Lorem ipsum dolor sit amet, consectetuer adipiscing elit");
    
            // show the list of long words after the processing is done
            System.out.println(String.join(", ", longWords));
            // consectetuer, adipiscing
        }
    }




## Observer using IObservable and IObserver (C#)
[`IObserver<T>`][1] and [`IObservable<T>`][1] interfaces can be used to implement observer pattern in .NET

 - `IObservable<T>` interface represents the class that sends notifications
 - `IObserver<T>` interface represents the class that receives them


<!-- language: c# -->

    public class Stock {
      private string Symbol { get; set; }
      private decimal Price { get; set; }
    }

    public class Investor : IObserver<Stock> {
      public IDisposable unsubscriber;
      public virtual void Subscribe(IObservable<Stock> provider) {
        if(provider != null) {
          unsubscriber = provider.Subscribe(this);
        }
      }
      public virtual void OnCompleted() {
        unsubscriber.Dispose();
      }
      public virtual void OnError(Exception e) {
      }
      public virtual void OnNext(Stock stock) {
      }
    }

    public class StockTrader : IObservable<Stock> {
      public StockTrader() {
        observers = new List<IObserver<Stock>>();
      }
      private IList<IObserver<Stock>> observers;
      public IDisposable Subscribe(IObserver<Stock> observer) {
        if(!observers.Contains(observer)) {
          observers.Add(observer);
        }
        return new Unsubscriber(observers, observer);
      }
      public class Unsubscriber : IDisposable {
        private IList<IObserver<Stock>> _observers;
        private IObserver<Stock> _observer;

        public Unsubscriber(IList<IObserver<Stock>> observers, IObserver<Stock> observer) {
          _observers = observers;
          _observer = observer;
        }

        public void Dispose() {
          Dispose(true);
        }
        private bool _disposed = false;
        protected virtual void Dispose(bool disposing) {
          if(_disposed) {
            return;
          }
          if(disposing) {
            if(_observer != null && _observers.Contains(_observer)) {
              _observers.Remove(_observer);
            }
          }
          _disposed = true;
        }
      }
      public void Trade(Stock stock) {
        foreach(var observer in observers) {
          if(stock== null) {
            observer.OnError(new ArgumentNullException());
          }
          observer.OnNext(stock);
        }
      }
      public void End() {
        foreach(var observer in observers.ToArray()) {
          observer.OnCompleted();
        }
        observers.Clear();
      }
    }


Usage

    ...
    var provider = new StockTrader();
    var i1 = new Investor();
    i1.Subscribe(provider);
    var i2 = new Investor();
    i2.Subscribe(provider);

    provider.Trade(new Stock());
    provider.Trade(new Stock());
    provider.Trade(null);
    provider.End();
    ...

REF: [Design patterns and practices in .NET: the Observer pattern][2]

  [1]: https://msdn.microsoft.com/en-us/library/dd990377(v=vs.110).aspx
  [2]: https://dotnetcodr.com/2013/08/01/design-patterns-and-practices-in-net-the-observer-pattern/




