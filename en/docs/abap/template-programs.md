---
title: "Template Programs"
slug: "template-programs"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- CLASS DEFINITION ABSTRACT FINAL makes the program class essentially static as instance methods could never be used. The intention is to keep the class minimal.

## OO Program with essential event methods
    REPORT z_template.
    
    CLASS lcl_program DEFINITION ABSTRACT FINAL.
    
      PUBLIC SECTION.
    
        CLASS-METHODS start_of_selection.
        CLASS-METHODS initialization.
        CLASS-METHODS end_of_selection.
    
    ENDCLASS.
    
    CLASS lcl_program IMPLEMENTATION.
    
      METHOD initialization.
    
      ENDMETHOD.
    
      METHOD start_of_selection.
    
      ENDMETHOD.
    
      METHOD end_of_selection.
    
      ENDMETHOD.
    
    ENDCLASS.
    
    INITIALIZATION.
    
      lcl_program=>initialization( ).
    
    START-OF-SELECTION.
    
      lcl_program=>start_of_selection( ).
    
    END-OF-SELECTION.
    
      lcl_program=>end_of_selection( ).

