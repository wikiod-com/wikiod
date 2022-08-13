---
title: "Unit testing"
slug: "unit-testing"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Structure of a test class
Test classes are created as local classes in a special unit test include.

This is the basic structure of a test class:

    CLASS lcl_test DEFINITION
                   FOR TESTING
                   DURATION SHORT
                   RISK LEVEL HARMLESS.
  
    PRIVATE SECTION.
      DATA:
        mo_cut TYPE REF TO zcl_dummy.
  
      METHODS:
        setup,
  
      "********* 30 chars *********|
      dummy_test                     for testing.
    ENDCLASS.

    CLASS lcl_test IMPLEMENTATION.
      METHOD setup.
        CREATE OBJECT mo_cut.
      ENDMETHOD.
    
      METHOD dummy_test.
        cl_aunit_assert=>fail( ).
      ENDMETHOD.
    ENDCLASS.

Any method declared with `FOR TESTING` will be a unit test. `setup` is a special method that is executed before each test.

## Separate data access from logic
An important principle for unit testing is to separate data access from business logic. One efficient technique for this is to define interfaces for data access. Your main class always use a reference to that interface instead of direct reading or writing data.

in production code the main class will be given an object that wraps actual data access. This could be select statement, function mudule calls, anything really. The important part is that this class should not perform anything else. No logic.

When testing the main class, you give it an object that serves static, fake data instead.

**An example for accessing the `SCARR` table**

*Data access interface `ZIF_DB_SCARR`:*

    INTERFACE zif_db_scarr
      PUBLIC.
        METHODS get_all
          RETURNING
            VALUE(rt_scarr) TYPE scarr_tab .
    ENDINTERFACE.

*Fake data class and test class:*

    CLASS lcl_db_scarr DEFINITION.
      PUBLIC SECTION.
        INTERFACES: zif_db_scarr.
    ENDCLASS.

    CLASS lcl_db_scarr IMPLEMENTATION.
      METHOD zif_db_scarr~get_all.
        " generate static data here
      ENDMETHOD.
    ENDCLASS.

    CLASS lcl_test DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

      PRIVATE SECTION.
        DATA:
          mo_cut TYPE REF TO zcl_main_class.

        METHODS:
          setup.
    ENDCLASS.

    CLASS lcl_test IMPLEMENTATION.
      METHOD setup.
        DATA: lo_db_scarr TYPE REF TO lcl_db_scarr.

        CREATE OBJECT lo_db_scarr.

        CREATE OBJECT mo_cut
          EXPORTING
            io_db_scarr = lo_db_scarr.
      ENDMETHOD.
    ENDCLASS.

The idea here is that in production code, `ZCL_MAIN_CLASS` will get a `ZIF_DB_SCARR` object that does a `SELECT` and returns the whole table while the unit test runs against a static dataset defined right there in the unit test include.

