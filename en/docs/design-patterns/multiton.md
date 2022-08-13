---
title: "Multiton"
slug: "multiton"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

**Multitonitis**

Same as [Singleton][1], Multiton can be considered a bad practice. However, there are times when you can use it wisely (for example, if you building a system like ORM/ODM to persist multiple objects).

[1]: https://www.wikiod.com/design-patterns/singleton

## Pool of Singletons (PHP example)
Multiton can be used as a container for singletons. This is Multiton implementation is a combination of Singleton and Pool patterns.

This is an example of how common Multiton abstract Pool class can be created: 

    abstract class MultitonPoolAbstract
    {
        /**
         * @var array
         */
        protected static $instances = [];
    
        final protected function __construct() {}

        /**
         * Get class name of lately binded class
         *
         * @return string
         */
        final protected static function getClassName()
        {
            return get_called_class();
        }
    
        /**
         * Instantiates a calling class object
         *
         * @return static
         */
        public static function getInstance()
        {
            $className = static::getClassName();
    
            if( !isset(self::$instances[$className]) ) {
                self::$instances[$className] = new $className;
            }
    
            return self::$instances[$className];
        }
    
        /**
         * Deletes a calling class object
         *
         * @return void
         */
        public static function deleteInstance()
        {
            $className = static::getClassName();
    
            if( isset(self::$instances[$className]) )
                unset(self::$instances[$className]);
        }
    
        /*-------------------------------------------------------------------------
        | Seal methods that can instantiate the class
        |------------------------------------------------------------------------*/
    
        final protected function __clone() {}
    
        final protected function __sleep() {}
    
        final protected function __wakeup() {}
    }

This way we can instantiate a various Singleton pools.

## Registry of Singletons (PHP example)
This pattern can be used to contain a registered Pools of Singletons, each distinguished by unique ID:

    abstract class MultitonRegistryAbstract
    {
        /**
         * @var array
         */
        protected static $instances = [];
    
        /** 
         * @param string $id
         */
        final protected function __construct($id) {}
    
        /**
         * Get class name of lately binded class
         *
         * @return string
         */
        final protected static function getClassName()
        {
            return get_called_class();
        }
    
        /**
         * Instantiates a calling class object
         *
         * @return static
         */
        public static function getInstance($id)
        {
            $className = static::getClassName();
    
            if( !isset(self::$instances[$className]) ) {
                self::$instances[$className] = [$id => new $className($id)];
            } else {
                if( !isset(self::$instances[$className][$id]) ) {
                    self::$instances[$className][$id] = new $className($id);
                }
            }
    
            return self::$instances[$className][$id];
        }
    
        /**
         * Deletes a calling class object
         *
         * @return void
         */
        public static function unsetInstance($id)
        {
            $className = static::getClassName();
    
            if( isset(self::$instances[$className]) ) {
                if( isset(self::$instances[$className][$id]) ) {
                    unset(self::$instances[$className][$id]);
                }
    
                if( empty(self::$instances[$className]) ) {
                    unset(self::$instances[$className]);
                }
            }
        }
    
        /*-------------------------------------------------------------------------
        | Seal methods that can instantiate the class
        |------------------------------------------------------------------------*/
    
        final protected function __clone() {}
    
        final protected function __sleep() {}
    
        final protected function __wakeup() {}
    }

This is simplified form of pattern that can be used for ORM to store several entities of a given type.

