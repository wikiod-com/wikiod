---
title: "Reflection"
slug: "reflection"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## What is an Assembly?
Assemblies are the building block of any [Common Language Runtime (CLR)][CLR] application.
Every type you define, together with its methods, properties and their bytecode, is compiled and packaged inside an Assembly.

    using System.Reflection;

<b></b>
    
    Assembly assembly = this.GetType().Assembly;   
    
Assemblies are self-documenting: they do not only contain types, methods and their IL code, but also the Metadata necessary to inspect and consume them, both at compile and runtime:

    Assembly assembly = Assembly.GetExecutingAssembly();

    foreach (var type in assembly.GetTypes())
    {
        Console.WriteLine(type.FullName);
    }
 
Assemblies have names which describes their full, unique identity:

    Console.WriteLine(typeof(int).Assembly.FullName);
    // Will print: "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

If this name includes a `PublicKeyToken`, it is called a *strong name*. Strong-naming an assembly is the process of creating a signature by using the private key that corresponds to the public key distributed with the assembly. This signature is added to the Assembly manifest, which contains the names and hashes of all the files that make up the assembly, and its `PublicKeyToken` becomes part of the name. Assemblies that have the same strong name should be identical; strong names are used in versioning and to prevent assembly conflicts.

[CLR]: https://en.wikipedia.org/wiki/Common_Language_Runtime

## Compare two objects with reflection
    public class Equatable
    {
        public string field1;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            var type = obj.GetType();
            if (GetType() != type)
                return false;

            var fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                if (field.GetValue(this) != field.GetValue(obj))
                    return false;

            return true;
        }

        public override int GetHashCode()
        {
            var accumulator = 0;
            var fields = GetType().GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                accumulator = unchecked ((accumulator * 937) ^ field.GetValue(this).GetHashCode());

            return accumulator;
        }
    }

**Note:** this example do a field based comparasion (ignore static fields and properties) for simplicity


## How to create an object of T using Reflection
Using the default constructor

    T variable = Activator.CreateInstance(typeof(T));


Using parameterized constructor

    T variable = Activator.CreateInstance(typeof(T), arg1, arg2);

## Creating Object and setting properties using reflection
Lets say we have a class `Classy` that has property Propertua 

    public class Classy
    {
        public string Propertua {get; set;}
    }

to set `Propertua` using reflection:

    var typeOfClassy = typeof (Classy);
    var classy = new Classy();
    var prop = typeOfClassy.GetProperty("Propertua");
    prop.SetValue(classy, "Value");

## Getting an attribute of an enum with reflection (and caching it)
Attributes can be useful for denoting metadata on enums. Getting the value of this can be slow, so it is important to cache results.

        private static Dictionary<object, object> attributeCache = new Dictionary<object, object>();

        public static T GetAttribute<T, V>(this V value)
            where T : Attribute
            where V : struct
        {
            object temp;

            // Try to get the value from the static cache.
            if (attributeCache.TryGetValue(value, out temp))
            {
                return (T) temp;
            }
            else
            {
                // Get the type of the struct passed in.
                Type type = value.GetType();   
                FieldInfo fieldInfo = type.GetField(value.ToString());

                // Get the custom attributes of the type desired found on the struct.
                T[] attribs = (T[])fieldInfo.GetCustomAttributes(typeof(T), false);

                // Return the first if there was a match.
                var result = attribs.Length > 0 ? attribs[0] : null;

                // Cache the result so future checks won't need reflection.
                attributeCache.Add(value, result);

                return result;
            }
        }

