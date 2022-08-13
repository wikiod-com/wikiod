---
title: "Singleton"
slug: "singleton"
draft: false
images: []
weight: 9821
type: docs
toc: true
---

The Singleton design pattern is sometimes regarded as "*Anti pattern*". This is due to the fact that it has some problems. 
You have to decide for yourself if you think it is appropriate to use it.
This topic has been discussed several times on StackOverflow.

See:
http://stackoverflow.com/questions/137975/what-is-so-bad-about-singletons

## Singleton (Java)
Singletons in Java are very similar to C#, being as both languages are object orientated. Below is an example of a singleton class, where only one version of the object can be alive during the program's lifetime (Assuming the program works on one thread)

<!-- language: java -->
    public class SingletonExample {
    
        private SingletonExample() { }
    
        private static SingletonExample _instance;
    
        public static SingletonExample getInstance() {
    
            if (_instance == null) {
                _instance = new SingletonExample();
            }
            return _instance;
        }
    }

Here is the thread safe version of that program:

<!-- language: java -->
    public class SingletonThreadSafeExample {
    
        private SingletonThreadSafeExample () { }
    
        private static volatile SingletonThreadSafeExample _instance;
    
        public static SingletonThreadSafeExample getInstance() {
            if (_instance == null) {
                    createInstance();
            }
            return _instance;
        }

        private static void createInstance() {
            synchronized(SingletonThreadSafeExample.class) {
                if (_instance == null) {
                    _instance = new SingletonThreadSafeExample();
                }
            }
        }
    }

Java also have an object called `ThreadLocal`, which creates a single instance of an object on a thread by thread basis. This could be useful in applications where each thread need its own version of the object

<!-- language: java -->
    public class SingletonThreadLocalExample {
    
        private SingletonThreadLocalExample () { }
    
        private static ThreadLocal<SingletonThreadLocalExample> _instance = new ThreadLocal<SingletonThreadLocalExample>();
    
        public static SingletonThreadLocalExample getInstance() {
            if (_instance.get() == null) {
                _instance.set(new SingletonThreadLocalExample());
            }
            return _instance.get();
        }
    }

Here is also a **Singleton** implementation with `enum` (containing only one element):

<!-- language: java -->
    public enum SingletonEnum {
        INSTANCE;
        // fields, methods
    }
Any **Enum** class implementation ensures that there is *only one* instance of its each element will exist.


**Bill Pugh Singleton Pattern**

Bill Pugh Singleton Pattern is the most widely used approach for Singleton class as it doesn’t require synchronization

<!-- language: java -->
    public class SingletonExample {
    
        private SingletonExample(){}
        
        private static class SingletonHolder{
            private static final SingletonExample INSTANCE = new SingletonExample();
        }
        
        public static SingletonExample getInstance(){
            return SingletonHolder.INSTANCE;
        }
    }

with using private inner static class the holder is not loaded to memory until someone call the getInstance method. Bill Pugh solution is thread safe and it doesn't required synchronization.

-----------------------

There are more Java singleton examples in the https://www.wikiod.com/java/singletons topic under the Java documentation tag.

## Singleton (C#)
Singletons are used to ensure that only one instance of an object is being created. The singleton allows only a single instance of itself to be created which means it controls its creation. 
The singleton is one of the [Gang of Four](https://en.wikipedia.org/wiki/Design_Patterns) design patterns and is a **creational pattern**.

# Thread-Safe Singleton Pattern

<!-- language: c# -->

<!-- language: c# -->
    public sealed class Singleton
    {
        private static Singleton _instance;
        private static object _lock = new object();
     
        private Singleton()
        {
        }
     
        public static Singleton GetSingleton()
        {
            if (_instance == null)
            {
                 CreateSingleton();
            }
 
            return _instance;
        }

        private static void CreateSingleton()
        {
            lock (_lock )
            {
                if (_instance == null)
                {
                     _instance = new Singleton();
                }
            }
        }
    }

[Jon Skeet](http://csharpindepth.com/Articles/General/Singleton.aspx) provides the following implementation for a lazy, thread-safe singleton:

<!-- language: c# -->
    public sealed class Singleton
    {
        private static readonly Lazy<Singleton> lazy =
            new Lazy<Singleton>(() => new Singleton());
        
        public static Singleton Instance { get { return lazy.Value; } }
    
        private Singleton()
        {
        }
    } 

## C# Example: Multithreaded Singleton
Static initialization is suitable for most situations. When your application must delay the instantiation, use a non-default constructor or perform other tasks before the instantiation, and work in a multithreaded environment, you need a different solution. Cases do exist, however, in which you cannot rely on the common language runtime to ensure thread safety, as in the Static Initialization example. In such cases, you must use specific language capabilities to ensure that only one instance of the object is created in the presence of multiple threads. One of the more common solutions is to use the Double-Check Locking [Lea99] idiom to keep separate threads from creating new instances of the singleton at the same time. 

The following implementation allows only a single thread to enter the critical area, which the lock block identifies, when no instance of Singleton has yet been created:

    using System;
    
    public sealed class Singleton {    
       private static volatile Singleton instance;    
       private static object syncRoot = new Object();
    
       private Singleton() {}
    
       public static Singleton Instance    {
          get 
          {
             if (instance == null) 
             {
                lock (syncRoot) 
                {
                   if (instance == null) 
                      instance = new Singleton();
                }
             }
    
             return instance;
          }    
      } 
    }

This approach ensures that only one instance is created and only when the instance is needed. Also, the variable is declared to be volatile to ensure that assignment to the instance variable completes before the instance variable can be accessed. Lastly, this approach uses a syncRoot instance to lock on, rather than locking on the type itself, to avoid deadlocks.

This double-check locking approach solves the thread concurrency problems while avoiding an exclusive lock in every call to the Instance property method. It also allows you to delay instantiation until the object is first accessed. In practice, an application rarely requires this type of implementation. In most cases, the static initialization approach is sufficient.

Reference: MSDN
 
**Acknowledgments**

[Gamma95] Gamma, Helm, Johnson, and Vlissides. Design Patterns: Elements of Reusable Object-Oriented Software. Addison-Wesley, 1995.

[Lea99] Lea, Doug. Concurrent Programming in Java, Second Edition. Addison-Wesley, 1999.

[Sells03] Sells, Chris. "Sealed Sucks." sellsbrothers.com News. Available at: http://www.sellsbrothers.com/news/showTopic.aspx?ixTopic=411.

## Lazy Singleton practical example in java
Real life use cases for Singleton pattern;

If you are developing a client-server application, you need single instrance of `ConnectionManager`, which manages the life cycle of client connections. 


The basic APIs in ConnectionManager :

`registerConnection`: Add new connection to existing list of connections

`closeConnection` : Close the connection either from event triggered by Client or Server

`broadcastMessage` : Some times, you have to send a message to all subscribed client connections. 

I am not providing complete implementation of source code since example will become very lengthy. At high level, the code will be like this.

    import java.util.*;
    import java.net.*;
    
    /* Lazy Singleton - Thread Safe Singleton without synchronization and volatile constructs */
    final class  LazyConnectionManager {
        private Map<String,Connection> connections = new HashMap<String,Connection>();
        private LazyConnectionManager() {}
        public static LazyConnectionManager getInstance() {
            return LazyHolder.INSTANCE;
        }
        private static class LazyHolder {
            private static final LazyConnectionManager INSTANCE = new LazyConnectionManager();
        }

        /* Make sure that De-Serailzation does not create a new instance */
        private Object readResolve()  {
            return LazyHolder.INSTANCE;
        }
        public void registerConnection(Connection connection){
            /* Add new connection to list of existing connection */
            connections.put(connection.getConnectionId(),connection);
        }
        public void closeConnection(String connectionId){
            /* Close connection and remove from map */
            Connection connection = connections.get(connectionId);
            if ( connection != null) {
                connection.close();
                connections.remove(connectionId);
            }
        }
        public void broadcastMessage(String message){
            for (Map.Entry<String, Connection> entry : connections.entrySet()){
                entry.getValue().sendMessage(message);            
            }
        }    
    }

Sample Server class:

    class Server implements Runnable{
        ServerSocket socket;
        int id;
        public Server(){
            new Thread(this).start();
        }
        public void run(){
            try{
                ServerSocket socket = new ServerSocket(4567);
                while(true){
                    Socket clientSocket = socket.accept();
                    ++id;
                    Connection connection = new Connection(""+ id,clientSocket);
                    LazyConnectionManager.getInstance().registerConnection(connection);    
                    LazyConnectionManager.getInstance().broadcastMessage("Message pushed by server:");
                }
            }catch(Exception err){
                err.printStackTrace();
            }
        }
        
    }

Other practical use cases for Singletons:

1. Managing global resources like `ThreadPool, ObjectPool, DatabaseConnectionPool` etc.
2. Centralised services like `Logging` application data with different log levels like `DEBUG,INFO,WARN,ERROR` etc 
3. Global `RegistryService` where different services are registered with a central component on startup.That global service can act as a `Facade` for the application 


## Singleton (PHP)
Example from [phptherightway.com][1]
```php
<?php
class Singleton
{
    /**
     * @var Singleton The reference to *Singleton* instance of this class
     */
    private static $instance;
    
    /**
     * Returns the *Singleton* instance of this class.
     *
     * @return Singleton The *Singleton* instance.
     */
    public static function getInstance()
    {
        if (null === static::$instance) {
            static::$instance = new static();
        }
        
        return static::$instance;
    }

    /**
     * Protected constructor to prevent creating a new instance of the
     * *Singleton* via the `new` operator from outside of this class.
     */
    protected function __construct()
    {
    }

    /**
     * Private clone method to prevent cloning of the instance of the
     * *Singleton* instance.
     *
     * @return void
     */
    private function __clone()
    {
    }

    /**
     * Private unserialize method to prevent unserializing of the *Singleton*
     * instance.
     *
     * @return void
     */
    private function __wakeup()
    {
    }
}

class SingletonChild extends Singleton
{
}

$obj = Singleton::getInstance();
var_dump($obj === Singleton::getInstance());             // bool(true)

$anotherObj = SingletonChild::getInstance();
var_dump($anotherObj === Singleton::getInstance());      // bool(false)

var_dump($anotherObj === SingletonChild::getInstance()); // bool(true)
```


  [1]: http://www.phptherightway.com/pages/Design-Patterns.html#singleton

## Singleton (C++)
As per [Wiki][1] : In software engineering, the singleton pattern is a design pattern that restricts the instantiation of a class to one object. 

This is required to create exactly one object to coordinate actions across the system.

    class Singleton
    {
        // Private constructor so it can not be arbitrarily created.
        Singleton()
        {}
        // Disable the copy and move
        Singleton(Singleton const&)            = delete;
        Singleton& operator=(Singleton const&) = delete;
      public:

        // Get the only instance
        static Singleton& instance()
        {
            // Use static member.
            // Lazily created on first call to instance in thread safe way (after C++ 11)
            // Guaranteed to be correctly destroyed on normal application exit.
            static Singleton _instance;

            // Return a reference to the static member.
            return _instance;
        }
    };

[1]: https://en.wikipedia.org/wiki/Singleton_pattern

## Singleton Design pattern (in general)
Note: The singleton is a design pattern.  
**But** it also considered an anti-pattern.

The use of a singleton should be considered carefully before use. There are usually better alternatives.

The main problem with a singleton is the same as the problem with global variables. They introduce external global mutable state. This means that functions that use a singleton are not solely dependent on the input parameters but also the state of the singleton. This means that testing can be severely compromised (difficult).

The issues with singletons can be mitigated by using them in conjunction with creation patterns; so that the initial creation of the singleton can be controlled.

