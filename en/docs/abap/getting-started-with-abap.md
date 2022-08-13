---
title: "Getting started with ABAP"
slug: "getting-started-with-abap"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
    PROGRAM zhello_world.
    START-OF-SELECTION.
        WRITE 'Hello, World!'.

Instead of printing to the console, ABAP writes values to a list which will be displayed as soon as the main logic was executed.

## Hello World in ABAP Objects
    PROGRAM zhello_world.

    CLASS main DEFINITION FINAL CREATE PRIVATE.
      PUBLIC SECTION.
        CLASS-METHODS: start.
    ENDCLASS.
    
    CLASS main IMPLEMENTATION.
      METHOD start.
        cl_demo_output=>display( 'Hello World!' ).
      ENDMETHOD.
    ENDCLASS.
    
    START-OF-SELECTION.
      main=>start( ).

