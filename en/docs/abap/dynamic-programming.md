---
title: "Dynamic Programming"
slug: "dynamic-programming"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Field-Symbols
Field-Symbols are ABAP's equivalent to pointers, except that Field-Symbols are always dereferenced (it is not possible to change the actual address in memory).

**Declaration**

To declare a Field-Symbol the keyword `FIELD-SYMBOLS` must be used. Types can be generic (`ANY [... TABLE]`) to handle a wide variety of variables.

    FIELD-SYMBOLS: <fs_line>     TYPE any,    "generic
                   <fs_struct>   TYPE kna1.   "non-generic

**Assigning**

Field-Symbols are `unassigned` on declaration, which means that they are pointing to nothing. Accessing an unassigned Field-Symbol will lead to an exception, and, if uncaught, to a short dump. Therefore, the state should be checked with `IS ASSIGNED`:

    IF <fs> IS ASSIGNED.
    *... symbol is assigned
    ENDIF.

As they are only references, no real data can be stored inside. So, declared `DATA` is needed in every case of use.

    DATA: w_name  TYPE string VALUE `Max`,
          w_index TYPE i      VALUE 1.
     
    FIELD-SYMBOLS <fs_name> TYPE any.

    ASSIGN w_name TO <fs_name>. "<fs_name> now gets w_name
    <fs_name> = 'Manni'.        "Changes to <fs_name> now also affect w_name

    * As <fs_name> is generic, it can also be used for numbers

    ASSIGN w_index TO <fs_name>. "<fs_name> now refers to w_index.    
    ADD 1 TO <fs_name>.          "w_index gets incremented by one

**Unassigning**

Sometimes it could be useful to reset a Field-Symbol. This can be done using `UNASSIGN`.

    UNASSIGN <fs>.
    * Access on <fs> now leads to an exception again

**Use for internal tables**

Field-Symbols may be used to modify internal tables.

    LOOP AT itab INTO DATA(wa).
    * Only modifies wa_line
        wa-name1 = 'Max'. 
    ENDLOOP.    

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
    * Directly refers to a line of itab and modifies its values
        <fs>-name1 = 'Max'. 
    ENDLOOP.

**Attention!** Field-Symbols stay assigned even after leaving the loop. If you want to reuse them safely, unassign them immediately.

## Data references
Essential for data references is the addition `REF TO` after `TYPE`. 

**Dynamic Creation of Structures**

If the type of a structure should be decided on runtime, we can define our target structure as reference to the generic type `data`.

    DATA wa TYPE REF TO data.

To give `wa` a type we use the statement `CREATE DATA`. The addition `TYPE` can be specified by:

Reference:

> `CREATE DATA wa TYPE kna1`
> 
> - *Static checks are active so it's not possible to create an unknown type* 

Name:

> `CREATE DATA wa TYPE (lw_name_as_string)` 
> 
> - *The parentheses are needed and `lw_name_as_string` contains the types name as string.*
> - *If the type was not found, an exception of type `CX_SY_CREATE_DATA_ERROR` will be thrown*


For instancing dynamically created types the `HANDLE` addition can be specified. `HANDLE` receives an object which inherits from `CL_ABAP_DATADESCR`.

> `CREATE DATA dref TYPE HANDLE obj`
> - `obj` can be created using the **R**un**T**ime **T**ype **S**ervices
> - because `dref` is still a datareference, it has to be dereferenced (`->*`) to be used as datacontainer (normally done via Field-Symbols)


## RunTime Type Services
RunTime Type Services (*short:* **RTTS**) are used either for:
- creating types (RunTime Type Creation; *short:* **RTTC**) 
- analysing types (RunTime Type Identification; *short:* **RTTI**)

**Classes**

    CL_ABAP_TYPEDESCR 
      | 
      |--CL_ABAP_DATADESCR 
      |   | 
      |   |--CL_ABAP_ELEMDESCR 
      |   |--CL_ABAP_REFDESCR 
      |   |--CL_ABAP_COMPLEXDESCR 
      |       | 
      |       |--CL_ABAP_STRUCTDESCR 
      |       |--CL_ABAP_TABLEDESCR 
      | 
      |--CL_ABAP_OBJECTDESCR 
         | 
         |--CL_ABAP_CLASSDESCR 
         |--CL_ABAP_INTFDESCR

`CL_ABAP_TYPEDESCR` is the base class. It implements the needed methods for describing:

   - `DESCRIBE_BY_DATA`
   - `DESCRIBE_BY_NAME`
   - `DESCRIBE_BY_OBJECT_REF`
   - `DESCRIBE_BY_DATA_REF`


