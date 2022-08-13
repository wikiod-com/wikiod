---
title: "Function add_action()"
slug: "function-add_action"
draft: false
images: []
weight: 9889
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
| $tag | *(string) (Required)* The name of the action to which the `$function_to_add` is hooked |
| $function_to_add | *([callable](http://php.net/manual/en/language.types.callable.php)) (Required)* The function which should be called when the action indicated by `$tag` is executed |
| $priority  | *(int) (Optional) Default value: `10`* Used to specify the order in which the functions associated with a particular action are executed. Lower numbers correspond with earlier execution, and functions with the same priority are executed in the order in which they were added to the action. |
| $accepted_args | *(int) (Optional) Default value: `1`* The number of arguments the function accepts. |

The `add_action()` function creates an **Action Hook**, associating a PHP function with a particular *action* "tag" or name. When the action is "triggered" by a call to `do_action()` (or `do_action_ref_array()`) with a specific tag, all functions "hooked" to that tag will be executed.

In most cases, this function should be used in a theme's `functions.php` file or a plugin file - or another source file loaded by either.

This function is a part of the **Plugin API**

## Basic Action Hook
The most basic application of `add_action()` is to add custom code to be executed at a certain location in a WordPress installation's source-code  - either using actions supplied by the **Core** of WordPress, or ones created by third-party code such as plugins and themes.

To add content to the `<head></head>` section of the site - say to a add `<link>` meta element to indicate where copyright information for the site can be found - `add_action()` can be used to attach a function that prints the appropriate markup to the `'wp_head'` action (which "triggers" when WordPress builds the `<head>` section):

    function add_copyright_meta_link() {
      echo( '<link rel="copyright" href="' . get_home_url() . '/copyright">' );
    }

    add_action( 'wp_head', 'add_copyright_meta_link' );

## Action Hook Priority
Any number of functions may be "hooked" to any given action. In some instances it is important for a hooked function to execute before or after others, which is where the third parameter to `add_action()`, `$priority` comes into play.

If the `$priority` argument is omitted, the function will be attached with the default priority of `10`. When the action is "triggered", the "hooked" functions will be called starting with those added with the smallest `$priority`, and progressing to the functions with the largest `$priority`. Any hooked functions that share the same priority will be called in the order that they were added (the order in which their respective `add_action()` calls were executed).

For instance, say a third-party plugin is using a function hooked to the `'template_redirect'` action in order to forward visitors to the `daily-deal` page to an affiliate link for an external e-commerce site, but you'd like the redirection to only occur for logged-in users. You would need to use your own `'template_redirect'` hook to send logged-out visitors to the sign-in page. After determining that the third-party plugin attaches it's function with the default `$piority` of `10`, you could hook your function with a priority of `9` to ensure that your logged-in check happens first:

    function redirect_deal_visitors_to_login() {
      if( is_page( 'daily-deal' ) && !user_is_logged_in() ) {
        wp_redirect( wp_login_url() );
        exit();
      }
    }

    add_action( 'template_redirect', 'redirect_deal_visitors_to_login', 9 );

## Hooking Class & Object Methods to Actions
[PHP Classes](http://php.net/manual/en/language.oop5.basic.php) are powerful tool for improving code organization and minimizing naming collisions. At some point or another, the question of how to create an action hook for a class method inevitably arises.

The `$function_to_add` argument is often shown as a string containing the function's name, however the data-type of the argument is actually a "[callable](http://php.net/manual/en/language.types.callable.php)", which for our purposes can be summed up as "a reference to a function or method".

There are a number of callable formats that can be used to reference methods on classes and objects. In all cases however, the referenced method *must* be publicly [visible](http://php.net/manual/en/language.oop5.visibility.php). A method is public when it is either prefixed with the `public` keyword, or no visibility keyword at all (in which case the method defaults to public).

# Object Method Action Hooks #
Object methods are executed on a particular instance of a class.

    class My_Class {
      // Constructor
      function My_Class() {
        // (Instantiation logic)
      }

      // Initialization function
      public function initialize() {
        // (Initialization logic)
      }
    }

After instantiating the above class as follows,

    $my_class_instance = new My_Class();

the `initialize()` method would normally be invoked on the object by calling `$my_class_instance->initialize();`. Hooking the method to the `'init'` WordPress action is done by passing an array containing a reference to the instance and a string containing the object method's name:

    add_action( 'init', [ $my_class_instance, 'initialize' ] );

If `add_action()` is called within an object method, the `$this` pseudo-variable can also be used:

    class My_Class {
      // Constructor
      function My_Class() {
        // (Instantiation logic)
        add_action( 'init', [ $this, 'initialize' ] );
      }
    
      // Initialization function
      public function initialize() {
        // (Initialization logic)
      }
    }

-----
    
# Class Method Action Hooks #
Class methods are executed statically on a class rather than any particular instance. Given the following class,

    class My_Class {   
      // Initialization function
      public static function initialize() {
        // (Initialization logic)
      }
    }

the `initialize()` method would normally be invoked by using the `::` scope-resolution operator, i.e. `My_Class::initialize();`. Hooking a static class method to a WordPress can be done in a couple different ways:

 - Using an array composed of a string containing the class name, and a string containing the method name:

       add_action( 'init', [ 'My_Class', 'initialize' ] );

 - Passing a string containing a full reference to the method, including the `::` operator:

       add_action( 'init', 'My_Class::initialize' );

 - If `add_action()` is called within a static class method, the `self` keyword or the `__CLASS__` magic-constant can be used in place of the class name. Note that this is generally inadvisable as the values of these items become somewhat counter-intuitive in the case of class inheritance.

       class My_Class {
         // Setup function
         public static function setup_actions() {
           add_action( 'init', 'self::initialize' );
         }
   
         // Initialization function
         public static function initialize() {
           // (Initialization logic)
         }
       }



