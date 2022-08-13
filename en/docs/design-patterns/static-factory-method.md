---
title: "Static factory method"
slug: "static-factory-method"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Static Factory Method C#
The *static factory method* is a variation of the *factory method* pattern.
It is used to create objects without having to call the constructor yourself.

**When to use the Static Factory Method**
- if you want to give a meaningfull name to the method that generates your object. 
- if you want to avoid overcomplex object creation see [Tuple Msdn][1].
- if you want to limit the number of objects created (caching)
- if you want to return an object of any subtype of their return type.


**There are some disadvantages like**
- Classes without a public or protected constructor cannot be initialized in the static factory method.
- Static factory methods are like normal static methods, so they are not distinguishable from other static methods (this may vary from IDE to IDE)
 

**Example**

Pizza.cs

    public class Pizza
    {
        public int SizeDiameterCM
        {
            get;
            private set;
        }
             
        private Pizza()
        {
            SizeDiameterCM = 25;
        }

        public static Pizza GetPizza()
        {
            return new Pizza();
        }

        public static Pizza GetLargePizza()
        {
            return new Pizza()
            {
                SizeDiameterCM = 35
            };
        }

        public static Pizza GetSmallPizza()
        {
            return new Pizza()
            {
                SizeDiameterCM = 28
            };
        }

        public override string ToString()
        {
            return String.Format("A Pizza with a diameter of {0} cm",SizeDiameterCM);
        }
    }

Program.cs

    class Program
    {
        static void Main(string[] args)
        {
            var pizzaNormal = Pizza.GetPizza();
            var pizzaLarge = Pizza.GetLargePizza();
            var pizzaSmall = Pizza.GetSmallPizza();


            String pizzaString = String.Format("{0} and {1} and {2}",pizzaSmall.ToString(), pizzaNormal.ToString(), pizzaLarge.ToString());
            Console.WriteLine(pizzaString);
        }
    }

**Output** 

> A Pizza with a diameter of 28 cm and A Pizza with a diameter of 25 cm
> and A Pizza with a diameter of 35 cm

  [1]: https://msdn.microsoft.com/en-gb/library/system.tuple%28v=vs.110%29.aspx

## Static Factory method
We can provide a meaningful name for our constructors.

We can provide several constructors with the same number and type of parameters, something that as we saw earlier we can’t do with class constructors.

    public class RandomIntGenerator {
        private final int min;
        private final int max;
     
        private RandomIntGenerator(int min, int max) {
            this.min = min;
            this.max = max;
        }
         
        public static RandomIntGenerator between(int max, int min) {
            return new RandomIntGenerator(min, max);
        }
         
        public static RandomIntGenerator biggerThan(int min) {
            return new RandomIntGenerator(min, Integer.MAX_VALUE);
        }
         
        public static RandomIntGenerator smallerThan(int max) {
            return new RandomIntGenerator(Integer.MIN_VALUE, max);
        }
     
        public int next() {...}
    }

## Hiding direct access to constructor
We can avoid providing direct access to resource intensive constructors, like for databases.
    public class DbConnection
    {
       private static final int MAX_CONNS = 100;
       private static int totalConnections = 0;
    
       private static Set<DbConnection> availableConnections = new HashSet<DbConnection>();
    
       private DbConnection()
       {
         // ...
         totalConnections++;
       }
    
       public static DbConnection getDbConnection()
       {
         if(totalConnections < MAX_CONNS)
         {
           return new DbConnection();
         }
    
         else if(availableConnections.size() > 0)
         {
             DbConnection dbc = availableConnections.iterator().next();
             availableConnections.remove(dbc);
             return dbc;
         }
    
         else {
           throw new NoDbConnections();
         }
       }
    
       public static void returnDbConnection(DbConnection dbc)
       {
         availableConnections.add(dbc);
         //...
       }
    }


