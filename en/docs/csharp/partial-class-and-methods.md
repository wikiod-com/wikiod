---
title: "Partial class and methods"
slug: "partial-class-and-methods"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Partial classes provides us an option to split classes into multiple parts and in multiple source files. All parts are combined into one single class during compile time. All parts should contain the keyword `partial`,should be of the same accessibility. All parts should be present in the same assembly for it to be included during compile time.

## Syntax
- public **partial** class MyPartialClass { }

- Partial classes must be defined within the same assembly, and namespace, as the class that they are extending.

- All parts of the class must use the `partial` keyword.

- All parts of the class must have the same accessibility; `public`/`protected`/`private` etc..

- If any part uses the `abstract` keyword, then the combined type is considered abstract.

- If any part uses the `sealed` keyword, then the combined type is considered sealed.

- If any part uses the a base type, then the combined type inherits from that type.

- The combined type inherits all the interfaces defined on all the partial classes.

## Partial classes
Partial classes provide an ability to split class declaration (usually into separate files). A common problem that can be solved with partial classes is allowing users to modify auto-generated code without fearing that their changes will be overwritten if the code is regenerated. Also multiple developers can work on same class or methods.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass
        {
            public void ExampleMethod() {
                Console.WriteLine("Method call from the first declaration.");
            }
        }
    
        public partial class PartialClass
        {
            public void AnotherExampleMethod()
            {
                Console.WriteLine("Method call from the second declaration.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.ExampleMethod(); // outputs "Method call from the first declaration."
                partial.AnotherExampleMethod(); // outputs "Method call from the second declaration."
            }
        }
    }

## Partial methods
Partial method consists of the definition in one partial class declaration (as a common scenario - in the auto-generated one) and the implementation in another partial class declaration.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass // Auto-generated
        {
            partial void PartialMethod();
        }
    
        public partial class PartialClass // Human-written
        {
            public void PartialMethod()
            {
                Console.WriteLine("Partial method called.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.PartialMethod(); // outputs "Partial method called."
            }
        }
    }

## Partial classes inheriting from a base class
When inheriting from any base class, only one partial class needs to have the base class specified.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass {}

You *can* specify the *same* base class in more than one partial class. It will get flagged as redundant by some IDE tools, but it does compile correctly.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass : BaseClass {} // base class here is redundant

You *cannot* specify *different* base classes in multiple partial classes, it will result in a compiler error.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {} // compiler error

    // PartialClass2.cs
    public partial class PartialClass : OtherBaseClass {} // compiler error

