---
title: "Singleton Pattern"
slug: "singleton-pattern"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

The singleton pattern has the goal to allow only one instance of a class to exists at any given time.

Preventing the direct instantiation via constructor is usually prevent by making it private. However, this is not possible in As3 and thus other ways to control the number of instances have to be used.

## Singleton enforcer via private instance
In this approach, the single is accessed via the static method:

    Singleton.getInstance();

To enforce only one instance of the singleton, a private static variable retains the instance, while any additional attempts to instantiate an instance are enforced within the constructor.  

    package {
    
    public class Singleton {
    
        /** Singleton instance */
        private static var _instance: Singleton = new Singleton();
    
        /** Return singleton instance. */
        public static function getInstance():Singleton {
            return _instance;
        }
    
        /** Constructor as singleton enforcer. */
        public function Singleton() {
            if (_instance)
                throw new Error("Singleton is a singleton and can only be accessed through Singleton.getInstance()");
        }

    }
    }

