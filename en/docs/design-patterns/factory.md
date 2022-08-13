---
title: "Factory"
slug: "factory"
draft: false
images: []
weight: 9711
type: docs
toc: true
---

>Provide an interface for creating families of related or dependent objects without specifying their concrete classes.
>
> -- *GOF 1994*

## Simple factory (Java)
A factory decreases coupling between code that needs to create objects from object creation code. Object creation is not made explicitly by calling a class constructor but by calling some function that creates the object on behalf the caller. A simple Java example is the following one:
<!-- language: lang-java -->

    interface Car {
    }

    public class CarFactory{
      static public Car create(String s) {
        switch (s) {
        default:
        case "us":
        case "american": return new Chrysler();
        case "de":
        case "german": return new Mercedes();
        case "jp":
        case "japanese": return new Mazda();
        }
      }
    }

    class Chrysler implements Car {
      public String toString() { return "Chrysler"; }
    }

    class Mazda implements Car {
      public String toString() { return "Mazda"; }
    }

    class Mercedes implements Car {
      public String toString() { return "Mercedes"; }
    }

    public class CarEx {
      public static void main(String args[]) {
        Car car = CarFactory.create("us");
        System.out.println(car);
      }
    }

In this example, user just gives some hint about what he needs and the factory is free to construct something appropriate. It is a **dependency inversion**: the implementor of `Car` concept is free to return an appropriate concrete `Car` requested by the user which in turn doesn't know the details of the concrete object built.

This is a simple example of how factory works, of course in this example it is always possible to instantiate concrete classes; but one can prevent it by hiding concrete classes in a package, such that user is forced to use the factory.
  
[.Net Fiddle][1] for above example.


  [1]: https://dotnetfiddle.net/Gtd7DP

## Factory example by implementing Factory method (Java)
***Intent:***

Define an interface for creating an object, but let sub classes decide which class to instantiate. Factory Method lets a class defer instantiation to sub classes.

UML diagram:

[![enter image description here][1]][1]

*Product:* It defines an interface of the objects the Factory method creates.

*ConcreteProduct:* Implements Product interface

*Creator:* Declares the Factory method

*ConcreateCreator:*  Implements the Factory method to return an instance of a ConcreteProduct

Problem statement: Create a Factory of Games by using Factory Methods, which defines the game interface.

Code snippet:

<!-- language: java -->

    import java.util.HashMap;


    /* Product interface as per UML diagram */
    interface Game{
        /* createGame is a complex method, which executes a sequence of game steps */
        public void createGame();
    }
    
    /* ConcreteProduct implementation as per UML diagram */
    class Chess implements Game{
        public Chess(){
            createGame();
        }
        public void createGame(){
            System.out.println("---------------------------------------");
            System.out.println("Create Chess game");
            System.out.println("Opponents:2");
            System.out.println("Define 64 blocks");
            System.out.println("Place 16 pieces for White opponent");
            System.out.println("Place 16 pieces for Black opponent");
            System.out.println("Start Chess game");
            System.out.println("---------------------------------------");
        }
    }
    class Checkers implements Game{
        public Checkers(){
            createGame();
        }
        public void createGame(){
            System.out.println("---------------------------------------");
            System.out.println("Create Checkers game");
            System.out.println("Opponents:2 or 3 or 4 or 6");
            System.out.println("For each opponent, place 10 coins");
            System.out.println("Start Checkers game");
            System.out.println("---------------------------------------");
        }
    }
    class Ludo implements Game{
        public Ludo(){
            createGame();
        }
        public void createGame(){
            System.out.println("---------------------------------------");
            System.out.println("Create Ludo game");
            System.out.println("Opponents:2 or 3 or 4");
            System.out.println("For each opponent, place 4 coins");
            System.out.println("Create two dices with numbers from 1-6");
            System.out.println("Start Ludo game");
            System.out.println("---------------------------------------");
        }
    }
    
    /* Creator interface as per UML diagram */
    interface IGameFactory {
        public Game getGame(String gameName);
    }
    
    /* ConcreteCreator implementation as per UML diagram */
    class GameFactory implements IGameFactory {
            
        HashMap<String,Game> games = new HashMap<String,Game>();
        /*  
            Since Game Creation is complex process, we don't want to create game using new operator every time.
            Instead we create Game only once and store it in Factory. When client request a specific game, 
            Game object is returned from Factory instead of creating new Game on the fly, which is time consuming
        */
        
        public GameFactory(){
            
            games.put(Chess.class.getName(),new Chess());
            games.put(Checkers.class.getName(),new Checkers());
            games.put(Ludo.class.getName(),new Ludo());        
        }
        public Game getGame(String gameName){
            return games.get(gameName);
        }
    }
    
    public class NonStaticFactoryDemo{
        public static void main(String args[]){
            if ( args.length < 1){
                System.out.println("Usage: java FactoryDemo gameName");
                return;
            }
         
            GameFactory factory = new GameFactory();
            Game game = factory.getGame(args[0]);
            System.out.println("Game="+game.getClass().getName());
        }
    }
    
output:
    
    java NonStaticFactoryDemo Chess
    ---------------------------------------
    Create Chess game
    Opponents:2
    Define 64 blocks
    Place 16 pieces for White opponent
    Place 16 pieces for Black opponent
    Start Chess game
    ---------------------------------------
    ---------------------------------------
    Create Checkers game
    Opponents:2 or 3 or 4 or 6
    For each opponent, place 10 coins
    Start Checkers game
    ---------------------------------------
    ---------------------------------------
    Create Ludo game
    Opponents:2 or 3 or 4
    For each opponent, place 4 coins
    Create two dices with numbers from 1-6
    Start Ludo game
    ---------------------------------------
    Game=Chess

This example shows a `Factory` class by implementing a `FactoryMethod`.

1. `Game` is the interface for all type of games. It defines complex method: `createGame()`

2. `Chess, Ludo, Checkers` are different variants of games, which provide implementation to `createGame()`

3. `public Game getGame(String gameName)` is `FactoryMethod` in `IGameFactory` class

4. `GameFactory` pre-creates different type of games in constructor. It implements `IGameFactory` factory method. 

5. game Name is passed as command line argument to `NotStaticFactoryDemo`

6. `getGame` in `GameFactory` accepts a game name and returns corresponding `Game` object.

When to use:

1. ***Factory***: When you don't want to expose object instantiation logic to the client/caller
2. ***Abstract Factory***: When you want to provide interface to families of related or dependent objects without specifying their concrete classes
3. ***Factory Method:***  To define an interface for creating an object, but let the sub-classes decide which class to instantiate

Comparison with other creational patterns:

1. Design start out using **Factory Method** (less complicated, more customizable, subclasses proliferate) and evolve toward **Abstract Factory, Prototype, or Builder** (more flexible, more complex) as the designer discovers where more flexibility is needed  

2. **Abstract Factory** classes are often implemented with **Factory Methods**, but they can also be implemented using **Prototype**

References for further reading: [Sourcemaking design-patterns][2]


  [1]: http://i.stack.imgur.com/HF19B.gif
  [2]: https://sourcemaking.com/design_patterns

## An Abstract Factory


## Abstract factory (C++)
**Abstract factory** pattern provides a way to obtain an coherent collection of objects through a collection of factories functions. As for every pattern, coupling is reduced by abstracting the way a set of objects are created, so that the user code is unaware of the many details of the objects he needs.

The following C++ example illustrates how to obtain different kind of objects of the same (hypothetical) GUI family:
<!-- language: c++ -->

    #include <iostream>

    /* Abstract definitions */
    class GUIComponent {
    public:
      virtual ~GUIComponent() = default;
      virtual void draw() const = 0;
    };
    class Frame  : public GUIComponent {};
    class Button : public GUIComponent {};
    class Label  : public GUIComponent {};

    class GUIFactory {
    public:
      virtual ~GUIFactory() = default;
      virtual std::unique_ptr<Frame> createFrame() = 0;
      virtual std::unique_ptr<Button> createButton() = 0;
      virtual std::unique_ptr<Label> createLabel() = 0;
      static std::unique_ptr<GUIFactory> create(const std::string& type);
    };

    /* Windows support */
    class WindowsFactory : public GUIFactory {
    private:
        class WindowsFrame : public Frame {
        public:
          void draw() const override { std::cout << "I'm a Windows-like frame" << std::endl; }
        };
        class WindowsButton : public Button {
        public:
          void draw() const override { std::cout << "I'm a Windows-like button" << std::endl; }
        };
        class WindowsLabel : public Label {
        public:
          void draw() const override { std::cout << "I'm a Windows-like label" << std::endl; }
        };
    public:
      std::unique_ptr<Frame> createFrame() override { return std::make_unique<WindowsFrame>(); }
      std::unique_ptr<Button> createButton() override { return std::make_unique<WindowsButton>(); }
      std::unique_ptr<Label> createLabel() override { return std::make_unique<WindowsLabel>(); }
    };

    /* Linux support */
    class LinuxFactory : public GUIFactory {
    private:
        class LinuxFrame : public Frame {
        public:
          void draw() const override { std::cout << "I'm a Linux-like frame" << std::endl; }
        };
        class LinuxButton : public Button {
        public:
          void draw() const override { std::cout << "I'm a Linux-like button" << std::endl; }
        };
        class LinuxLabel : public Label {
        public:
          void draw() const override { std::cout << "I'm a Linux-like label" << std::endl; }
        };
    public:
      std::unique_ptr<Frame> createFrame() override { return std::make_unique<LinuxFrame>(); }
      std::unique_ptr<Button> createButton() override { return std::make_unique<LinuxButton>(); }
      std::unique_ptr<Label> createLabel() override { return std::make_unique<LinuxLabel>(); }
    };

    std::unique_ptr<GUIFactory> GUIFactory::create(const string& type) {
      if (type == "windows") return std::make_unique<WindowsFactory>();
      return std::make_unique<LinuxFactory>();
    }

    /* User code */
    void buildInterface(GUIFactory& factory) {
      auto frame = factory.createFrame();
      auto button = factory.createButton();
      auto label = factory.createLabel();

      frame->draw();
      button->draw();
      label->draw();
    }

    int main(int argc, char *argv[]) {
      if (argc < 2) return 1;
      auto guiFactory = GUIFactory::create(argv[1]);
      buildInterface(*guiFactory);
    }

If the generated executable is named `abstractfactory` then output may give:

    $ ./abstractfactory windows
    I'm a Windows-like frame
    I'm a Windows-like button
    I'm a Windows-like label
    $ ./abstractfactory linux  
    I'm a Linux-like frame
    I'm a Linux-like button
    I'm a Linux-like label


## Simple example of Factory that uses an IoC  (C#)
Factories can be used in conjunction with Inversion of Control (IoC) libraries too.

 - The typical use case for such a factory is when we want to create an object based on parameters that are not known until run-time (such as the current User). 
 - In these cases it can be sometimes be difficult (if not impossible) to configure the IoC library alone to handle this kind of runtime contextual information, so we can wrap it in a factory.

Example

 - Suppose we have a `User` class, whose characteristics (ID, Security clearance level, etc.), are unknown until runtime (since the current user could be anyone who uses the application).
 - We need to take the current User and obtain an `ISecurityToken` for them, which can then be used to check if the user is allowed to perform certain actions or not.
 - The implementation of ISecurityToken will vary depending on the level of the User - in other words, ISecurityToken uses *polymorphism*.

In this case, we have two implementations, which also use *Marker Interfaces* to make it easier to identify them to the IoC library; the IoC library in this case is just made up and identified by the abstraction `IContainer`.

> Note also that many modern IoC factories have native capabilities or plugins that allow auto-creation of factories as well as avoiding the need for marker interfaces as shown below; however since not all do, this example caters to a simple, lowest common functionality concept.

<!-- language-all: c# -->

    //describes the ability to allow or deny an action based on PerformAction.SecurityLevel
    public interface ISecurityToken
    {
        public bool IsAllowedTo(PerformAction action);
    }

    //Marker interface for Basic permissions
    public interface IBasicToken:ISecurityToken{};
    //Marker interface for super permissions
    public interface ISuperToken:ISecurityToken{};

    //since IBasictoken inherits ISecurityToken, BasicToken can be treated as an ISecurityToken
    public class BasicToken:IBasicToken
    {
         public bool IsAllowedTo(PerformAction action)
         {
             //Basic users can only perform basic actions
             if(action.SecurityLevel!=SecurityLevel.Basic) return false;
             return true;
         }
    }

    public class SuperToken:ISuperToken
    {
         public bool IsAllowedTo(PerformAction action)
         {
             //Super users can perform all actions         
             return true;
         }
    }

Next we will create a `SecurityToken` factory, which will take as a dependency our `IContainer`

    public class SecurityTokenFactory
    {
       readonly IContainer _container;
       public SecurityTokenFactory(IContainer container)
       {
          if(container==null) throw new ArgumentNullException("container");
       }
    
       public ISecurityToken GetToken(User user)
       {
          if (user==null) throw new ArgumentNullException("user);
          //depending on the user security level, we return a different type; however all types implement ISecurityToken so the factory can produce them.
          switch user.SecurityLevel
          {
              case Basic:
               return _container.GetInstance<BasicSecurityToken>();
              case SuperUser:
               return _container.GetInstance<SuperUserToken>();
          }
       }
    }

Once we've registered these with the `IContainer`:

    IContainer.For<SecurityTokenFactory>().Use<SecurityTokenFactory>().Singleton(); //we only need a single instance per app
    IContainer.For<IBasicToken>().Use<BasicToken>().PerRequest(); //we need an instance per-request
    IContainer.For<ISuperToken>().Use<SuperToken>().PerRequest();//we need an instance per-request 

the consuming code can use it to get the correct token at runtime:

    readonly SecurityTokenFactory _tokenFactory;
    ...
    ...
    public void LogIn(User user)
    {
        var token = _tokenFactory.GetToken(user);
        user.SetSecurityToken(token);
    }

> In this way we benefit from the encapsulation provided by the factory and also from the lifecycle management provided by the IoC library.


## Flyweight Factory (C#)
**In simple words:**

   A [Flyweight factory][1] that for a given, already known, key will always give the same object as response. For new keys will create the instance and return it.


**Using the factory:**

    ISomeFactory<string, object> factory = new FlyweightFactory<string, object>();

    var result1 = factory.GetSomeItem("string 1");
    var result2 = factory.GetSomeItem("string 2");
    var result3 = factory.GetSomeItem("string 1");

    //Objects from different keys
    bool shouldBeFalse = result1.Equals(result2);

    //Objects from same key
    bool shouldBeTrue = result1.Equals(result3);

**Implementation:**

    public interface ISomeFactory<TKey,TResult> where TResult : new()
    {
        TResult GetSomeItem(TKey key);
    }

    public class FlyweightFactory<TKey, TResult> : ISomeFactory<TKey, TResult> where TResult : new()
    {
        public TResult GetSomeItem(TKey key)
        {
            TResult result;
            if(!Mapping.TryGetValue(key, out result))
            {
                result = new TResult();
                Mapping.Add(key, result);
            }
            return result;
        }

        public Dictionary<TKey, TResult> Mapping { get; set; } = new Dictionary<TKey, TResult>();
    }

**Extra Notes**

I would recommend to add to this solution the use of an `IoC Container` (as explained in a different example here) instead of creating your own new instances. One can do it by adding a new registration for the `TResult` to the container and then resolving from it (instead of the `dictionary` in the example).

  [1]: http://www.dofactory.com/net/flyweight-design-pattern

## Factory method
The Factory method pattern is a creational pattern that abstracts away the instantiation logic of an object in order to decouple the client code from it.

When a factory method belongs to a class that is an implementation of another factory pattern such as [Abstract factory][1] then it is usually more appropriate to reference the pattern implemented by that class rather than the Factory method pattern.

The Factory method pattern is more commonly referenced when describing a factory method that belongs to a class which is not primarily a factory.

For instance, it may be advantageous to place a factory method on an object that represents a domain concept if that object encapsulates some state that would simplify the creation process of another object. A factory method may also lead to a design that is more aligned with the Ubiquitous Language of a specific context.

Here's a code example:

    //Without a factory method
    Comment comment = new Comment(authorId, postId, "This is a comment");
    
    //With a factory method
    Comment comment = post.comment(authorId, "This is a comment");


  [1]: https://www.wikiod.com/design-patterns/factory#Abstract factory (C++)

