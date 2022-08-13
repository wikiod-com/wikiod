---
title: "Strategy Pattern"
slug: "strategy-pattern"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

## Strategy pattern example in java with Context class
<!-- language: java -->

**Strategy:**

`Strategy` is a behavioural pattern, which allows to change the algorithm dynamically from a family of related algorithms. 

UML of Strategy pattern from Wikipedia

[![enter image description here][1]][1]:



<!-- language: java -->
    import java.util.*;
    
    /* Interface for Strategy */
    interface OfferStrategy {
        public String getName();
        public double getDiscountPercentage();
    }
    /* Concrete implementation of base Strategy */
    class NoDiscountStrategy implements OfferStrategy{
        public String getName(){
            return this.getClass().getName();
        }
        public double getDiscountPercentage(){
            return 0;
        }
    }
    /* Concrete implementation of base Strategy */
    class QuarterDiscountStrategy implements OfferStrategy{
        public String getName(){
            return this.getClass().getName();
        }
        public double getDiscountPercentage(){
            return 0.25;
        }
    }
    /* Context is optional. But if it is present, it acts as single point of contact
       for client. 
       
       Multiple uses of Context
       1. It can populate data to execute an operation of strategy
       2. It can take independent decision on Strategy creation. 
       3. In absence of Context, client should be aware of concrete strategies. Context acts a wrapper and hides internals
       4. Code re-factoring will become easy
    */
    class StrategyContext {
        double price; // price for some item or air ticket etc.
        Map<String,OfferStrategy> strategyContext = new HashMap<String,OfferStrategy>();
        StrategyContext(double price){
            this.price= price;
            strategyContext.put(NoDiscountStrategy.class.getName(),new NoDiscountStrategy());
            strategyContext.put(QuarterDiscountStrategy.class.getName(),new QuarterDiscountStrategy());        
        }
        public void applyStrategy(OfferStrategy strategy){
            /* 
            Currently applyStrategy has simple implementation. You can Context for populating some more information,
            which is required to call a particular operation            
            */
            System.out.println("Price before offer :"+price);
            double finalPrice = price - (price*strategy.getDiscountPercentage());
            System.out.println("Price after offer:"+finalPrice);
        }
        public OfferStrategy getStrategy(int monthNo){
            /*
                In absence of this Context method, client has to import relevant concrete Strategies everywhere.
                Context acts as single point of contact for the Client to get relevant Strategy
            */
            if ( monthNo < 6 )  {
                return strategyContext.get(NoDiscountStrategy.class.getName());
            }else{
                return strategyContext.get(QuarterDiscountStrategy.class.getName());
            }
            
        }
    }
    public class StrategyDemo{    
        public static void main(String args[]){
            StrategyContext context = new StrategyContext(100);
            System.out.println("Enter month number between 1 and 12");
            int month = Integer.parseInt(args[0]);
            System.out.println("Month ="+month);
            OfferStrategy strategy = context.getStrategy(month);
            context.applyStrategy(strategy);
        }
        
    }


output:

    Enter month number between 1 and 12
    Month =1
    Price before offer :100.0
    Price after offer:100.0
    
    Enter month number between 1 and 12
    Month =7
    Price before offer :100.0
    Price after offer:75.0

Problem statement: Offer 25% discount on price of item for the months of July-December. Do not provide any discount for the months of Jan-June.

Above example shows the usage of `Strategy` pattern with `Context`. `Context` can be used as Single Point of Contact for the `Client`. 

Two Strategies - `NoOfferStrategy` and `QuarterDiscountStrategy` have been declared as per problem statement.

As shown in the output column, you will get discount depending on the month you have entered

**Use case(s) for Strategy pattern**:

1. Use this pattern when you have a family of interchangeable algorithms and you have to change the algorithm at run time. 

2. Keep the code cleaner by removing conditional statements

  [1]: http://i.stack.imgur.com/ASelh.png

## Using Java 8 functional interfaces to implement the Strategy pattern
The purpose of this example is to show how we can realize the Strategy pattern using Java 8 functional interfaces.  We will start with a simple use case codes in classic Java, and then recode it in the Java 8 way.

The example problem we using is a family of algorithms (strategies) that *describe* different ways to communicate over a distance.  

**The Classic Java version**
------------------------

The contract for our family of algorithms is defined by the following interface:

    public interface CommunicateInterface {
        public String communicate(String destination);
    }

Then we can implement a number of algorithms, as follows:

    public class CommunicateViaPhone implements CommunicateInterface {
        @Override
        public String communicate(String destination) {
            return "communicating " + destination +" via Phone..";
        }
    }
    
    public class CommunicateViaEmail implements CommunicateInterface {
        @Override
        public String communicate(String destination) {
            return "communicating " + destination + " via Email..";
        }
    }
    
    public class CommunicateViaVideo implements CommunicateInterface {
        @Override
        public String communicate(String destination) {
            return "communicating " + destination + " via Video..";
        }
    }

These can be instantiated as follows:

    CommunicateViaPhone communicateViaPhone = new CommunicateViaPhone();
    CommunicateViaEmail communicateViaEmail = new CommunicateViaEmail();
    CommunicateViaVideo communicateViaVideo = new CommunicateViaVideo();

Next, we implement a service that uses the strategy:

    public class CommunicationService {
        private CommunicateInterface communcationMeans;
    
        public void setCommuncationMeans(CommunicateInterface communcationMeans) {
            this.communcationMeans = communcationMeans;
        }
    
        public void communicate(String destination) {
            this.communcationMeans.communicate(destination);
        }
    }

Finally, we can use the different strategies as follows:

    CommunicationService communicationService = new CommunicationService();

    // via phone
    communicationService.setCommuncationMeans(communicateViaPhone);
    communicationService.communicate("1234567");

    // via email
    communicationService.setCommuncationMeans(communicateViaEmail);
    communicationService.communicate("hi@me.com");


**Using Java 8 functional interfaces**
----------------------------------

The contract of the different algorithm implementations does not need a dedicated interface.  Instead, we can describe it using the existing `java.util.function.Function<T, R>` interface.

The different algorithms composing `the family of algorithms` can be expressed as lambda expressions.  This replaces the strategy classes *and* their instantiations.

    Function<String, String> communicateViaEmail = 
            destination -> "communicating " + destination + " via Email..";
    Function<String, String> communicateViaPhone = 
            destination -> "communicating " + destination + " via Phone..";
    Function<String, String> communicateViaVideo = 
            destination -> "communicating " + destination + " via Video..";

Next, we can code the "service"  as follows:

    public class CommunicationService {
        private Function<String, String> communcationMeans;
    
        public void setCommuncationMeans(Function<String, String> communcationMeans) {
            this.communcationMeans = communcationMeans;
        }
    
        public void communicate(String destination) {
            this.communcationMeans.communicate(destination);
        }
    }

Finally we use the strategies as follows

    CommunicationService communicationService = new CommunicationService();

    // via phone
    communicationService.setCommuncationMeans(communicateViaPhone);
    communicationService.communicate("1234567");

    // via email
    communicationService.setCommuncationMeans(communicateViaEmail);
    communicationService.communicate("hi@me.com");

Or even:

    communicationService.setCommuncationMeans(
        destination -> "communicating " + destination + " via Smoke signals.." );
    CommunicationService.communicate("anyone");




## Hiding strategy implementation details


## Strategy pattern without a context class / Java
The following is a simple example of using the strategy pattern without a context class. There are two implementation strategies which implement the interface and solve the same problem in different ways. Users of the EnglishTranslation class can call the translate method and choose which strategy they would like to use for the translation, by specifying the desired strategy. 

<!-- language: java -->

    // The strategy interface
    public interface TranslationStrategy {
        String translate(String phrase);
    }
    
    // American strategy implementation 
    public class AmericanTranslationStrategy implements TranslationStrategy {
    
        @Override
        public String translate(String phrase) {
            return phrase + ", bro";
        }
    }

    // Australian strategy implementation     
    public class AustralianTranslationStrategy implements TranslationStrategy {
    
        @Override
        public String translate(String phrase) {
            return phrase + ", mate";
        }
    }
    
    // The main class which exposes a translate method
    public class EnglishTranslation {
    
        //  translate a phrase using a given strategy
        public static String translate(String phrase, TranslationStrategy strategy) {
            return strategy.translate(phrase);
        }
    
        // example usage
        public static void main(String[] args) {
    
            // translate a phrase using the AustralianTranslationStrategy class
            String aussieHello = translate("Hello", new AustralianTranslationStrategy());
            // Hello, mate

            // translate a phrase using the AmericanTranslationStrategy class    
            String usaHello = translate("Hello", new AmericanTranslationStrategy());
            // Hello, bro
        }
    }



## Strategy (PHP)
Example from [www.phptherightway.com][1]

```
<?php

interface OutputInterface
{
    public function load();
}

class SerializedArrayOutput implements OutputInterface
{
    public function load()
    {
        return serialize($arrayOfData);
    }
}

class JsonStringOutput implements OutputInterface
{
    public function load()
    {
        return json_encode($arrayOfData);
    }
}

class ArrayOutput implements OutputInterface
{
    public function load()
    {
        return $arrayOfData;
    }
}
```


  [1]: http://www.phptherightway.com/pages/Design-Patterns.html#strategy

