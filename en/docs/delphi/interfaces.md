---
title: "Interfaces"
slug: "interfaces"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

Interfaces are used to describe the needed information and the expected output of methods and classes, without providing information of the explicit implementation.

Classes can **implement** interfaces, and interfaces can **inherit** from each other. If a class is **implementing** an interface, this means all functions and procedures exposed by the interface exist in the class.

A special aspect of interfaces in delphi is that instances of interfaces have a lifetime management based on reference counting. The lifetime of class instances has to be managed manually.

Considering all these aspects, interfaces can be used to achieve different goals:

 - Provide multiple different implementations for operations (e.g. saving in a file, database or sending as E-Mail, all as Interface "SaveData")
 - Reduce dependencies, improving the decoupling and thus making the code better maintainable and testable
 - Work with instances in multiple units without getting troubled by lifetime management (though even here pitfalls exist, beware!)
  

 

## Defining and implementing an interface
An interface is declared like a class, but without access modifiers (`public`, `private`, ...). Also, no definitions are allowed, so variables and constants can't be used. 

Interfaces should always have an *Unique Identifier*, which can be generated by pressing <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>G</kbd>.

    IRepository = interface
        ['{AFCFCE96-2EC2-4AE4-8E23-D4C4FF6BBD01}']
        function  SaveKeyValuePair(aKey: Integer; aValue: string): Boolean;
    end;

To implement an interface, the name of the interface must be added behind the base class. Also, the class should be a descendant of `TInterfacedObject` (this is important for the *lifetime management*).

    TDatabaseRepository = class(TInterfacedObject, IRepository)
        function  SaveKeyValuePair(aKey: Integer; aValue: string): Boolean;
    end;


When a class implements an interface, it must include all methods and functions declared in the interface, else it won't compile. 

One thing worth noting is that access modifiers don't have any influence, if the caller works with the interface. For example all functions of the interface can be implemented as `strict private` members, but can still be called from another class if an instance of the interface is used.

## Properties in interfaces
Since the declaration of variables in interfaces isn't possible, the "fast" way of defining properites (`property Value: TObject read FValue write FValue;`) can't be used. Instead, the Getter and setter (each only if needed) have to be declared in the interface, too.

    IInterface = interface(IInterface)
        ['{6C47FF48-3943-4B53-8D5D-537F4A0DEC0D}']
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject;

        property Value: TObject read GetValue write SetValue;
    end;

One thing worth noting is that the implementing class doesn't have to declare the property. The compiler would accept this code:

    TImplementer = class(TInterfacedObject, IInterface)
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject
    end;

One caveat, however, is that this way the property can only be accessed through an instance of the interface, noth through the class itself. Also, adding the property  to the class increases the readability.

## Implementing multiple interfaces
Classes can implement more than one interface, as opposed to inheriting from more than one class (*Multiple Inheritance*) which isn't possible for Delphi classes. To achieve this, the name of all interfaces must be added comma-separated behind the base class. 

Of course, the implementing class must also define the functions declared by each of the interfaces. 

    IInterface1 = interface
        ['{A2437023-7606-4551-8D5A-1709212254AF}']
        procedure Method1();
        function Method2(): Boolean;
    end;

    IInterface2 = interface
        ['{6C47FF48-3943-4B53-8D5D-537F4A0DEC0D}']
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject;

        property Value: TObject read GetValue write SetValue;
    end;

    TImplementer = class(TInterfacedObject, IInterface1, IInterface2)
        // IInterface1
        procedure Method1();
        function Method2(): Boolean;

        // IInterface2
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject

        property Value: TObject read GetValue write SetValue;
    end;



## Inheritance for interfaces
Interfaces can inherit from each other, exactly like classes do, too. An implementing class thus has to implement functions of the interface and all base interfaces. This way, however, the compiler doesn't know that the implenting class also implements the base interface, it only knows of the interfaces that are explicitly listed. That's why using `as ISuperInterface` on `TImplementer` wouldn't work. That also results in the common practice, to explicitly implement all base interfaces, too (in this case `TImplementer = class(TInterfacedObject, IDescendantInterface, ISuperInterface)`).

    ISuperInterface = interface
        ['{A2437023-7606-4551-8D5A-1709212254AF}']
        procedure Method1();
        function Method2(): Boolean;
    end;

    IDescendantInterface = interface(ISuperInterface)
        ['{6C47FF48-3943-4B53-8D5D-537F4A0DEC0D}']
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject;

        property Value: TObject read GetValue write SetValue;
    end;

    TImplementer = class(TInterfacedObject, IDescendantInterface)
        // ISuperInterface
        procedure Method1();
        function Method2(): Boolean;

        // IDescendantInterface
        procedure SetValue(const aValue: TObject);
        function  GetValue(): TObject

        property Value: TObject read GetValue write SetValue;
    end;

