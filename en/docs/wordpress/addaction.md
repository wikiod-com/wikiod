---
title: "add_action()"
slug: "add_action"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - add_action( $tag, $function_to_add )
 - add_action( $tag, $function_to_add, $priority )
 - add_action( $tag, $function_to_add, $priority, $accepted_args )

## Parameters
| Parameter | Details |
| --------- | ------- |
| *$tag*   | *(string)* The name of the action to which the procedure `$function_to_add` will be hooked. |
| *$function_to_add* | *(callable)* The callable function/procedure you want to be called. |
| *$priority* | *(int)* The priority level to which the `$function_to_add` will be executed. Optional. Default 10.|
| *$accepted_args* | *(int)* The number of arguments that the callable function `$function_to_add` accepts. Optional. Default 1.| 

## Direct function callback
    add_action( 'init', function() {
        // do something here
    } );
Using a function block to hook a set of instructions. With the `init` hook, the set of instructions will be executed right after wordpress has finished loading the necessary components.

## Function name reference callback
    function my_init_function() {
        // do something here
    }

    add_action( 'init', 'my_init_function' );
Using the name of the function to hook a set of instructions. With the `init` hook, the set of instructions will be executed right after wordpress has finished loading the necessary components.

## Class static method callback
    class MyClass {
        static function my_init_method() {
            // do something here
        }
    }

    add_action( 'init', array( 'MyClass', 'my_init_method' ) );
Using a static method of a class to hook a set of instructions. With the `init` hook, the set of instructions will be executed right after wordpress has finished loading the necessary components.

## Object method callback
    class MyClass {
        function my_init_method() {
            // do something here
        }
    }
    
    $obj = new MyClass();
    
    add_action( 'init', array( $obj, 'my_init_method' ) );

Using a method of an object to hook a set of instructions. With the `init` hook, the set of instructions will be executed right after wordpress has finished loading the necessary components.

