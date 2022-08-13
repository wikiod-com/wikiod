---
title: "Using the Proxy Class"
slug: "using-the-proxy-class"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

First, I have to say. There **is** a reason this Proxy thing, despite its seeming usefulness, is not highlighted enough on the Internet.

You **cannot** use it to watch a random property of a random class/instance. You are only allowed to use this technique by subclassing the **Proxy** class.

Some operations do not expect exceptions so you need to completely understand what are you doing and why are you doing it, and your code **must** be absolutely clean and error-less.

## Implementation
The other thing about Proxy class, and why it is not so popular, is that it is rather difficult to fathom a problem that exactly needs a dynamic class with a controllable access to its dynamic properties and methods as a most appropriate solution. Every time I tried to use Proxy, I have ended up resorting to something else, simpler and more controllable.

However, let us not be discouraged. I like the idea of addressing the last array elements by [-1], [-2], etc indices in **Python**. Might be not a big feat, but it feels nice to use that rather than long and clumsy **someArray[someArray.length - 1]**. Lets see what we can do about it.

    package
    {
        import flash.utils.Proxy;
        import flash.utils.flash_proxy;
        
        /**
         * Pyaray the Tentacled Whisperer of Impossible Secrets.
         */
        
        dynamic public class PyArray extends Proxy
        {
            private var data:Array;
            
            public function PyArray(...args:Array)
            {
                if (args.length == 0)
                {
                    data = new Array;
                }
                else if ((args.length == 1) && (args[0] is Array))
                {
                    data = args[0];
                }
                else
                {
                    data = args;
                }
            }
            
            // This is a getter proxy to all the available Array
            // elements and properties and, sometimes, methods.
            override flash_proxy function getProperty(name:*):*
            {
                var anIndex:int = name;
                
                // Handle the int indices of the Array elements.
                if (anIndex == name)
                {
                    // Handle the -1, -2, etc indexing.
                    if (anIndex < 0) anIndex += data.length;
                    
                    if (anIndex >= data.length) return null;
                    if (anIndex < 0) return null;
                    
                    return data[anIndex];
                }
                
                // Handle the existing public Array properties.
                if (data.hasOwnProperty(name)) return data[name];
                
                // Handle the Array methods addressed via ["member"] access.
                try
                {
                    if (data[name] is Function) return data[name];
                    else throw new Error;
                }
                catch (fail:Error)
                {
                    trace("[PyArray] is unable to resolve property \"" + name + "\".");
                }
                
                return null;
            }
            
            // This will set either elements, or settable properties.
            override flash_proxy function setProperty(name:*, value:*):void
            {
                var anIndex:int = name;
                
                // Handle the int indices of the Array elements.
                if (anIndex == name)
                {
                    // Handle the -1, -2, etc indexing.
                    if (anIndex < 0) anIndex += data.length;
                    
                    // In case the element index is out of range,
                    // the PyArray will extend its data Array.
                    // if (anIndex >= data.length) return;
                    if (anIndex < 0) return;
                    
                    data[anIndex] = value;
                    
                    return;
                }
                
                // Handle the existing (or dynamic) public Array properties.
                try
                {
                    data[name] = value;
                }
                catch (fail:Error)
                {
                    trace("[PyArray] is unable to set property \"" + name + "\".");
                }
                
                return;
            }
            
            // This allows to delete PyArray elements with "delete" operator.
            override flash_proxy function deleteProperty(name:*):Boolean
            {
                var anIndex:int = name;
                
                // Handle the int indices of the Array elements.
                if (anIndex == name)
                {
                    // Handle the -1, -2, etc indexing.
                    if (anIndex < 0) anIndex += data.length;
                    
                    if (anIndex >= data.length) return false;
                    if (anIndex < 0) return false;
                    
                    data.splice(anIndex, 1);
                    
                    return true;
                }
                
                // Handle the dynamic public Array properties.
                try
                {
                    delete data[name];
                    return true;
                }
                catch (fail:Error)
                {
                    trace("[PyArray] is unable to delete the \"" + name + "\" property.");
                }
                
                return false;
            }
            
            // This proxies any attempt to call a method on PyArray directly to data Array, thus
            // all Array methods (including "toString" method called through trace) are available.
            override flash_proxy function callProperty(name:*, ...rest):*
            {
                try
                {
                    return (data[name] as Function).apply(data, rest);
                }
                catch (fail:Error)
                {
                    trace("[PyArray] is unable to resolve method \"" + name + "\".");
                    return null;
                }
            }
            
            // This allows PyArray to handle for..in and for..each..in loops.
            // The initial call starts with zero, so we need to do this +1 -1 magic
            // in order for enumeration to work correctly. I'm not happy with this either.
            override flash_proxy function nextNameIndex(index:int):int
            {
                if (index >= data.length) return 0;
                else return index + 1;
            }
            
            // This method handles the for..in loop.
            override flash_proxy function nextName(index:int):String
            {
                return (index - 1).toString();
            }
            
            // This method handles the for..each..in loop.
            override flash_proxy function nextValue(index:int):*
            {
                return data[index - 1];
            }
        }
    }

## Usage
    package
    {
        import flash.display.Sprite;
        
        /**
         * Daemonette of Slaanesh.
         * 
         * It is minor female demon, vaguely human-like, but with crab-like pincers instead of hands.
         * She wears a rather indecent skimpy leather bikini, moves quickly and casts deadly spells!
         */
        
        public class Slaanesh extends Sprite
        {
            public function Slaanesh() 
            {
                // Lets initialize the PyArray.
                var PA:PyArray = new PyArray(1,2,3,4,5,4,3,2,1,"Foo");
                
                // Basic check: get the last element.
                trace(PA[-1]);
                // output:
                // Foo
                
                // This will map to the 0-based third element.
                trace(PA[2.0]);
                // output:
                // 3
                
                // This should not get us anywhere.
                trace(PA[2.1]);
                // output:
                // [PyArray] is unable to resolve property "2.1".
                // null
                
                // This should return the length of the data Array.
                trace(PA["length"]);
                // output:
                // 10
                
                // This should return the length of the data Array.
                // This will not compile unless PyArray class is marked "dynamic".
                trace(PA.length);
                // output:
                // 10
                
                // This will map to indexOf method of data Array via getProperty method.
                trace(PA["indexOf"]);
                // output:
                // function Function() {}
                
                // This will map to indexOf method of data Array via getProperty method.
                // This will not compile unless PyArray class is marked "dynamic".
                trace(PA.indexOf);
                // output:
                // function Function() {}
                
                // This is a try to access a non-existent property.
                // This will not compile unless PyArray class is marked "dynamic".
                trace(PA.P124);
                // output:
                // [PyArray] is unable to resolve property "P124".
                // null
                
                // This is a try to call a non-existent method via callProperty method.
                // This will not compile unless PyArray class is marked "dynamic".
                trace(PA.P124());
                // output:
                // [PyArray] is unable to resolve method "P124".
                // null
                
                // Basic check: calling a proxied method via callProperty method.
                trace(PA.indexOf(5));
                // output:
                // 4
                
                // An attempt to replace an Array method with a random value.
                // This will not compile unless PyArray class is marked "dynamic".
                PA.indexOf = 123;
                // output:
                // [PyArray] is unable to set property "indexOf".
                
                // An attempt to assign a random value to a random property.
                // It will succees because Array, as a dynamic class, allows so.
                // This will not compile unless PyArray class is marked "dynamic".
                PA.indexOfz = 123;
                trace(PA.indexOfz);
                // output:
                // 123
                
                // An attempt to assign an Array element via negative indexing.
                // This trace works fine because toString method is also proxied.
                PA[-3] = "Hello";
                trace(PA);
                // output:
                // 1,2,3,4,5,4,3,Hello,1,Foo
                
                // An attempt to delete Array elements via normal and negative indexing.
                // This operation is mapped via deleteProperty method.
                delete PA[-4]; // deletes "3" before "Hello"
                delete PA[0];  // deletes "1" at the start.
                trace(PA);
                // output:
                // 2,3,4,5,4,Hello,1,Foo
                
                // An attempt to delete a non-dynamic method reference.
                // There's no error output, AS3 must be handling this internally.
                delete PA.indexOf;
                trace(PA.indexOf);
                // output:
                // function Function() {}
                
                // An attempt to set an element out of index range.
                PA[10] = "123abc";
                trace(PA);
                // output:
                // 2,3,4,5,4,Hello,1,Foo,,,123abc
                
                var aText:String;
                
                // This is a test of for..in loop, Array elements are
                // enumerated via nextName and nextNameIndex methods.
                aText = ""
                
                for (var aKey:String in PA)
                    aText += aKey + ":" + PA[aKey] + " ";
                
                trace(aText);
                // output:
                // 0:2 1:3 2:4 3:5 4:4 5:Hello 6:1 7:Foo 8:undefined 9:undefined 10:123abc 
                
                // This is a test of for..each..in loop, Array elements are
                // enumerated via nextValue and nextNameIndex methods.
                aText = "";
                
                for each (var aValue:* in PA)
                    aText += aValue + " ";
                
                trace(aText);
                // output:
                // 2 3 4 5 4 Hello 1 Foo undefined undefined 123abc
            }
        }
    }

