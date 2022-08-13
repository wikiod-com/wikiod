---
title: "Property Objects"
slug: "property-objects"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

**Note**: In Python 2, make sure that your class inherits from object (making it a new-style class) in order for all features of properties to be available.


## Using the @property decorator for read-write properties
If you want to use `@property` to implement custom behavior for setting and getting, use this pattern:

    class Cash(object):
        def __init__(self, value):
            self.value = value
        @property
        def formatted(self):
            return '${:.2f}'.format(self.value)
        @formatted.setter
        def formatted(self, new):
            self.value = float(new[1:])

To use this:

    >>> wallet = Cash(2.50)
    >>> print(wallet.formatted)
    $2.50
    >>> print(wallet.value)
    2.5
    >>> wallet.formatted = '$123.45'
    >>> print(wallet.formatted)
    $123.45
    >>> print(wallet.value)
    123.45



## Using the @property decorator
The `@property` decorator can be used to define methods in a class which act like attributes. One example where this can be useful is when exposing information which may require an initial (expensive) lookup and simple retrieval thereafter.

Given some module `foobar.py`:

```python
class Foo(object):
    def __init__(self):
        self.__bar = None

    @property
    def bar(self):
        if self.__bar is None:
            self.__bar = some_expensive_lookup_operation()
        return self.__bar

```

Then

```python
>>> from foobar import Foo
>>> foo = Foo()
>>> print(foo.bar)  # This will take some time since bar is None after initialization
42
>>> print(foo.bar)  # This is much faster since bar has a value now
42
```


## Overriding just a getter, setter or a deleter of a property object
When you inherit from a class with a property, you can provide a new implementation for one or more of the property `getter`, `setter` or `deleter` functions, by referencing the property object *on the parent class*:

    class BaseClass(object):
        @property
        def foo(self):
            return some_calculated_value()

        @foo.setter
        def foo(self, value):
            do_something_with_value(value)


    class DerivedClass(BaseClass):
        @BaseClass.foo.setter
        def foo(self, value):
            do_something_different_with_value(value)

You can also add a setter or deleter where there was not one on the base class before.

## Using properties without decorators
While using decorator syntax (with the @) is convenient, it also a bit concealing. You can use properties directly, without decorators. The following Python 3.x example shows this:

    class A:
        p = 1234
        def getX (self):
            return self._x

        def setX (self, value):
            self._x = value
                
        def getY (self):
            return self._y

        def setY (self, value):
            self._y = 1000 + value    # Weird but possible
            
        def getY2 (self):
            return self._y

        def setY2 (self, value):
            self._y = value
            
        def getT    (self):
            return self._t

        def setT (self, value):
            self._t = value
            
        def getU (self):
            return self._u + 10000

        def setU (self, value):
            self._u = value - 5000
                
        x, y, y2 = property (getX, setX), property (getY, setY), property (getY2, setY2)
        t = property (getT, setT)
        u = property (getU, setU)
        
    A.q = 5678

    class B:
        def getZ (self):
            return self.z_
        
        def setZ (self, value):
            self.z_ = value
            
        z = property (getZ, setZ)
        
    class C:
        def __init__ (self):
            self.offset = 1234

        def getW (self):
            return self.w_ + self.offset
            
        def setW (self, value):
            self.w_ = value - self.offset
            
        w = property (getW, setW)
        
    a1 = A ()
    a2 = A ()

    a1.y2 = 1000
    a2.y2 = 2000

    a1.x = 5
    a1.y = 6

    a2.x = 7
    a2.y = 8

    a1.t = 77
    a1.u = 88
        
    print (a1.x, a1.y, a1.y2)
    print (a2.x, a2.y, a2.y2)
    print (a1.p, a2.p, a1.q, a2.q)

    print (a1.t, a1.u)

    b = B ()
    c = C ()

    b.z = 100100
    c.z = 200200
    c.w = 300300

    print (a1.x, b.z, c.z, c.w)

    c.w = 400400
    c.z = 500500
    b.z = 600600

    print (a1.x, b.z, c.z, c.w)


