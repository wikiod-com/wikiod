---
title: "ABAP Objects"
slug: "abap-objects"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Class declaration
ABAP Classes can be declared **Globally** or **Locally**. A global class can be used by any object within the ABAP repository. By contrast, a local class can only be used within the scope it is declared.
----------


    CLASS lcl_abap_class DEFINITION.
      PUBLIC SECTION.
      PROTECTED SECTION.
      PRIVATE SECTION.
    ENDCLASS.
    
    CLASS lcl_abap_class IMPLEMENTATION.
    ENDCLASS.

## Constructor, methods
Class implementation:

    CLASS lcl_abap_class DEFINITION.
      PUBLIC SECTION.
        METHODS: constructor,
                 method1.
      PROTECTED SECTION.
      PRIVATE SECTION.
        METHODS: method2,
                 method3.
    ENDCLASS.
    
    CLASS lcl_abap_class IMPLEMENTATION.
        METHOD constructor.
            "Logic
        ENDMETHOD.

        METHOD method1.
            "Logic
        ENDMETHOD.

        METHOD method2.
            "Logic
            method3( ).
        ENDMETHOD.

        METHOD method3.
            "Logic
        ENDMETHOD.
    ENDCLASS.

Method call example:

    DATA lo_abap_class TYPE REF TO lcl_abap_class.
    CREATE OBJECT lo_abap_class. "Constructor call
    lo_abap_class->method1( ).



## Method with parameters (Importing, Changing, Exporting)
Class implementation:

    CLASS lcl_abap_class DEFINITION.
      PRIVATE SECTION.
        METHODS method1 IMPORTING iv_string TYPE string
                         CHANGING cv_string TYPE string
                        EXPORTING ev_string TYPE string.
    ENDCLASS.

    CLASS lcl_abap_class IMPLEMENTATION.
        METHOD method1.
            cv_string = iv_string.
            ev_string = 'example'.
        ENDMETHOD.
    ENDCLASS.

Method call example:

    method1 (
      EXPORTING iv_string = lv_string
      IMPORTING ev_string = lv_string2
       CHANGING cv_string = lv_string3
    ).



## Method with returning parameter
Class implementation:

    CLASS lcl_abap_class DEFINITION.
      PRIVATE SECTION.
        METHODS method1 RETURNING VALUE(rv_string) TYPE string.
    ENDCLASS.
    
    CLASS lcl_abap_class IMPLEMENTATION.
        METHOD method1.
            rv_string = 'returned value'.
        ENDMETHOD.
    ENDCLASS.

Method call example:

    lv_string = method1( ).

Note that parameters declared with `RETURNING` are passed by value only.

## Inheritance - definition
Information
-----------

> Inheritance allows you to derive a new class from an existing class.
> You do this using the **INHERITING** **FROM** addition in the
> 
> **CLASS** subclass **DEFINITION** **INHERITING** **FROM** superclass.
> 
> statement. The new class subclass inherits all of the components of
> the existing class superclass. The new class is called the subclass of
> the class from which it is derived. The original class is called the
> superclass of the new class. A class can have more than one direct subclass, but it may only have one direct superclass.

Class implementation
--------------------

    CLASS lcl_vehicle DEFINITION.
    ENDCLASS.
    
    CLASS lcl_vehicle IMPLEMENTATION.
    ENDCLASS.

    CLASS lcl_car DEFINITION INHERITING FROM lcl_vehicle.
    ENDCLASS.

    CLASS lcl_car IMPLEMENTATION.
    ENDCLASS.

## Inheritance - Abstract and Final Methods and Classes
Information
-----------

> The **ABSTRACT** and **FINAL** additions to the **METHODS** and
> **CLASS** statements allow you to define abstract and final methods or classes.
> 
> An abstract method is defined in an abstract class and cannot be
> implemented in that class. Instead, it is implemented in a subclass of
> the class. Abstract classes cannot be instantiated.
> 
> A final method cannot be redefined in a subclass. Final classes cannot
> have subclasses. They conclude an inheritance tree.


----------


Class implementation:
---------------------

    CLASS lcl_abstract DEFINITION ABSTRACT.
        PUBLIC SECTION.
            METHODS: abstract_method ABSTRACT,
                     final_method FINAL
                     normal_method.
                    
    ENDCLASS.

    CLASS lcl_abstract IMPLEMENTATION.
        METHOD final_method.
            "This method can't be redefined in child class!
        ENDMETHOD.

        METHOD normal_method.
            "Some logic
        ENDMETHOD.

            "We can't implement abstract_method here!

    ENDCLASS.

    CLASS lcl_abap_class DEFINITION INHERITING FROM lcl_abstract.
        PUBLIC SECTION.
            METHODS: abstract_method REDEFINITION,
                     abap_class_method.
    ENDCLASS.

    CLASS lcl_abap_class IMPLEMENTATION.
        METHOD abstract_method.
            "Abstract method implementation
        ENDMETHOD.

        METHOD abap_class_method.
            "Logic
        ENDMETHOD.
    ENDCLASS.

Method call example:
--------------------

    DATA lo_class TYPE REF TO lcl_abap_class.
    CREATE OBJECT lo_class.

    lo_class->abstract_method( ).
    lo_class->normal_method( ).
    lo_class->abap_class_method( ).
    lo_class->final_method( ).



