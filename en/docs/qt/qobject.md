---
title: "QObject"
slug: "qobject"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

`QObject` class is the base class for all Qt objects.

## qobject_cast
    T qobject_cast(QObject *object)

A functionality which is added by deriving from `QObject` and using the `Q_OBJECT` macro is the ability to use the `qobject_cast`. 

Example:

    class myObject : public QObject
    {
        Q_OBJECT
        //...
    };

    QObject* obj = new myObject();

To check whether `obj` is a `myObject`-type and to cast it to such in C++ you can generally use a [`dynamic_cast`](https://www.wikiod.com/docs/c%2b%2b/5660/casts#t=201609131207283939046). This is dependent on having RTTI enabled during compilation. 

The Q_OBJECT macro on the other hands generates the conversion-checks and code which can be used in the qobject_cast.

    myObject* my = qobject_cast<myObject*>(obj);
    if(!myObject)
    {
        //wrong type
    }

This is not reliant of RTTI. And also allows you to cast across dynamic library boundaries (via Qt interfaces/plugins).



## QObject example
`Q_OBJECT` macro appears in private section of a class. `Q_OBJECT` requires the class to be subclass of `QObject`. This macro is necessary for the class to declare its signals/slots and to use Qt meta-object system. 

If Meta Object Compiler (MOC) finds class with `Q_OBJECT`, it processes it and generates C++ source file containing meta object source code.

Here is the example of class header with `Q_OBJECT` and signal/slots:

    #include <QObject>

    class MyClass : public QObject
    {
        Q_OBJECT

    public:

    public slots:
        void setNumber(double number);        

    signals:
        void numberChanged(double number);

    private:
    } 

## QObject Lifetime and Ownership
QObjects come with their own alternative lifetime concept compared to native C++'s raw,unique or shared pointers.

QObjects have the possibility to build an objecttree by declaring parent/child relationships. 

The simplest way to declare this relationship is by passing the parent object in the constructor. As an  lternative you can manually set the parent of a `QObject` by calling `setParent`. This is the only direction to declare this relationship. You cannot add a child to a parents class but only the other way round.

    QObject parent;
    QObject child* = new QObject(&parent);

When `parent` now gets deleted in stack-unwind `child` will also be deleted.

When we delete a `QObject` it will "unregister" itself form the parent object;

    QObject parent;
    QObject child* = new QObject(&parent);
    delete child; //this causes no problem.

The same applies for stack variables:

    QObject parent;
    QObject child(&parent);

`child` will get deleted before `parent` during stack-unwind and unregister itself from it's parent. 

**Note:** You can manually call `setParent` with a reverse order of declaration which **will** break the automatic destruction.

