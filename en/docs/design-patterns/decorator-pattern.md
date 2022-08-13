---
title: "Decorator pattern"
slug: "decorator-pattern"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

Decorator pattern allows a user to add new functionality to an existing object without altering its structure. This type of design pattern comes under structural pattern as this pattern acts as a wrapper to existing class.

This pattern creates a decorator class which wraps the original class and provides additional functionality keeping class methods signature intact.

## Parameters


| Parameter | Description |
| ------ | ------ |
| Beverage| it can be Tea or Coffee   |

## VendingMachineDecorator
Definition of Decorator as per Wikiepdia:

The Decorator pattern can be used to extend (decorate) the functionality of a certain object statically, or in some cases at run-time, independently of other instances of the same class, provided some groundwork is done at design time.

Decorator attach additional responsibilities to an object dynamically. Decorators provide a flexible alternative to subclassing for extending functionality.

Decorator pattern contains four components.


[![enter image description here][1]][1]

1. Component Interface: It defines an interface to execute particular operations
2. ConcreteComponent: It implements the operations defined in Component interface
3. Decorator (Abstract) : it is an abstract class, which extends the component interface. It contains Component interface. In absence of this class, you need many sub-classes of ConcreteDecorators for different combinations. Composition of component reduces un-necessary sub-classes.
4. ConcreteDecorator: It holds the implementation of Abstract Decorator.

Coming back to example code,

1. *Beverage* is component. It defines an abstract method: decorateBeverage 
2. *Tea* and *Coffee* are concrete implementations of *Beverage*.
3. *BeverageDecorator* is an abstract class, which contains *Beverage*
4. SugarDecorator and LemonDecorator are concrete Decorators for BeverageDecorator.



**EDIT:** Changed the example to reflect real world scenario of computing the price of Beverage by adding one or more flavours like Sugar, Lemon etc( flavours are decorators) 

   
<!-- language: java -->

    abstract class Beverage {
        protected String name;
        protected int price;
        public Beverage(){
            
        }
        public  Beverage(String name){
            this.name = name;
        }
        public void setName(String name){
            this.name = name;
        }
        public String getName(){
            return name;
        }
        protected void setPrice(int price){
            this.price = price;
        }
        protected int getPrice(){
            return price;
        }
        protected abstract void decorateBeverage();
        
    }
    class Tea extends Beverage{
        public Tea(String name){
            super(name);
            setPrice(10);
        }
        public void decorateBeverage(){
            System.out.println("Cost of:"+ name +":"+ price);
            // You can add some more functionality
        }
    }
    class Coffee extends Beverage{
        public Coffee(String name){
            super(name);
            setPrice(15);
        }
        public void decorateBeverage(){
            System.out.println("Cost of:"+ name +":"+ price);
            // You can add some more functionality
        }    
    }
    abstract class BeverageDecorator extends Beverage {
        protected Beverage beverage;
        public BeverageDecorator(Beverage beverage){    
            this.beverage = beverage;    
            setName(beverage.getName()+"+"+getDecoratedName());
            setPrice(beverage.getPrice()+getIncrementPrice());
        }
        public void decorateBeverage(){
            beverage.decorateBeverage();
            System.out.println("Cost of:"+getName()+":"+getPrice());
        }    
        public abstract int getIncrementPrice();
        public abstract String getDecoratedName();
    }
    class SugarDecorator extends BeverageDecorator{
        public SugarDecorator(Beverage beverage){
            super(beverage);
        }
        public void decorateBeverage(){
            super.decorateBeverage();
            decorateSugar();        
        }
        public void decorateSugar(){
            System.out.println("Added Sugar to:"+beverage.getName());
        }
        public int getIncrementPrice(){
            return 5;
        }
        public String getDecoratedName(){
            return "Sugar";
        }
    }
    class LemonDecorator extends BeverageDecorator{
        public LemonDecorator(Beverage beverage){
            super(beverage);
        }
        public void decorateBeverage(){
            super.decorateBeverage();
            decorateLemon();    
        }
        public void decorateLemon(){
            System.out.println("Added Lemon to:"+beverage.getName());        
        }
        public int getIncrementPrice(){
            return 3;
        }
        public String getDecoratedName(){
            return "Lemon";
        }
    }
    
    public class VendingMachineDecorator {    
        public static void main(String args[]){
            Beverage beverage = new SugarDecorator(new LemonDecorator(new Tea("Assam Tea")));
            beverage.decorateBeverage();
            beverage = new SugarDecorator(new LemonDecorator(new Coffee("Cappuccino")));
            beverage.decorateBeverage();
        }
    }

    


output:

    Cost of:Assam Tea:10
    Cost of:Assam Tea+Lemon:13
    Added Lemon to:Assam Tea
    Cost of:Assam Tea+Lemon+Sugar:18
    Added Sugar to:Assam Tea+Lemon
    Cost of:Cappuccino:15
    Cost of:Cappuccino+Lemon:18
    Added Lemon to:Cappuccino
    Cost of:Cappuccino+Lemon+Sugar:23
    Added Sugar to:Cappuccino+Lemon


This example computes cost of beverage in Vending Machine after adding many flavours to the beverage.

In above example:

Cost of Tea = 10, Lemon = 3 and Sugar = 5. If you make Sugar + Lemon + Tea, it costs 18.

Cost of Coffee =15, Lemon = 3 and Sugar = 5. If you make Sugar + Lemon + Coffee, it costs 23

By using same Decorator for both beverages ( Tea and Coffee ), the number of sub-classes have been reduced. In absence of  Decorator pattern, you should have different sub classes for different combinations. 

The combinations will be like this:

    SugarLemonTea
    SugarTea
    LemonTea
    
    SugarLemonCapaccuino
    SugarCapaccuino
    LemonCapaccuino

etc. 

By using same `Decorator` for both beverages, the number of sub-classes have been reduced. It's possible due to `composition` rather than `inheritance` concept used in this pattern.

Comparison with other Design patterns ( From [sourcemaking][2] article)

1. *Adapter* provides a different interface to its subject. *Proxy* provides the same interface. *Decorator* provides an enhanced interface.

2. *Adapter* changes an object's interface, *Decorator* enhances an object's responsibilities. 

3. *Composite* and *Decorator* have similar structure diagrams, reflecting the fact that both rely on recursive composition to organize an open-ended number of objects

4. *Decorator* is designed to let you add responsibilities to objects without subclassing. *Composite's* focus is not on embellishment but on representation

5. *Decorator* and *Proxy* have different purposes but similar structures

6. *Decorator* lets you change the skin of an object. *Strategy* lets you change the guts.

**Key use cases:**

1. Add additional functionalities/responsibilities dynamically
2. Remove functionalities/responsibilities dynamically
3. Avoid too much of sub-classing to add additional responsibilities.



  [1]: http://i.stack.imgur.com/7JToL.png
  [2]: https://sourcemaking.com/design_patterns/decorator

## Caching Decorator
This example demonstrate how to add caching capabilities to `DbProductRepository` using Decorator pattern. This approach adheres to [SOLID principles][1] because it allows you to add caching without violating [Single responsibility principle][2] or [Open/closed principle][3].
<!-- language: c# -->

    public interface IProductRepository
    {
        Product GetProduct(int id);
    }
    
    public class DbProductRepository : IProductRepository
    {
        public Product GetProduct(int id)
        {
            //return Product retrieved from DB
        }
    }
    
    public class ProductRepositoryCachingDecorator : IProductRepository
    {
        private readonly IProductRepository _decoratedRepository;
        private readonly ICache _cache;
        private const int ExpirationInHours = 1;

        public ProductRepositoryCachingDecorator(IProductRepository decoratedRepository, ICache cache)
        {
            _decoratedRepository = decoratedRepository;
            _cache = cache;
        }

        public Product GetProduct(int id)
        {
            var cacheKey = GetKey(id);
            var product = _cache.Get<Product>(cacheKey);
            if (product == null)
            {
                product = _decoratedRepository.GetProduct(id);
                _cache.Set(cacheKey, product, DateTimeOffset.Now.AddHours(ExpirationInHours));
            }
            
            return product;
        }
    
        private string GetKey(int id) => "Product:" + id.ToString();
    }
    
    public interface ICache
    {
        T Get<T>(string key);
        void Set(string key, object value, DateTimeOffset expirationTime)
    }

Usage:

    var productRepository = new ProductRepositoryCachingDecorator(new DbProductRepository(), new Cache());
    var product = productRepository.GetProduct(1);

Result of invoking `GetProduct` will be: retrieve product from cache (decorator responsibility), if product was not in the cache proceed with invocation to `DbProductRepository`and retrieve product from DB. After this product can be added to the cache so subsequent calls won't hit DB.


  [1]: https://en.wikipedia.org/wiki/SOLID_(object-oriented_design)
  [2]: https://en.wikipedia.org/wiki/Single_responsibility_principle
  [3]: https://en.wikipedia.org/wiki/Open/closed_principle

