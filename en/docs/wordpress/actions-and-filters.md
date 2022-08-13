---
title: "Actions and Filters"
slug: "actions-and-filters"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
 - add_action( tag, function_to_call, priority, num_of_args );
 - add_filter( tag, function_to_call, priority, num_of_args );

## Parameters
| Parameter | Explanation |
| ------ | ------ |
|$tag| (string) (Required) The name of the action to which the $function is hooked.
| $function  | (callable) (Required) Requires a string containing the function name or anonymous function. See examples for adding functions within classes.   |
| $priority | (int) default = 10. Functions attached to hooks/ filters will run in the priority assigned. You may have a situation where you want to work with code before any other actions, set priority =1 or after all other attached functions priority = 100 etc. As with all php functions, you can use the function without passing a value for a variable where a default value has been set, but if you wish to change the number of parameters returned, you must specify! |
| $parameters | (int) default = 1. The number of parameters returned to your attached function. The parameters returned will depend on the number attached where the hook was created. See `apply_filters()` and `do_action()` for more details.   |

**Wordpress Hooks**

Something that often confuses developers when starting to work with WordPress are the use of `apply_filters()` and `add_action()`. You will often see plugins/themes making use of these in code and if you don't understand the concept, you will find it hard to work with them. 

In brief (very brief, look up WordPress load flowchart for process in detail), WordPress loads in the following way:

 1. wp-load.php - functions etc
 2. mu-plugins - any files found in the mu-plugins folder - often used to serve cached objects
 3. Plugins - no particular order, any installed and activated plugins will be loaded
 4. Active child theme / parent theme
 5. init - rest of data
 6. template

If you are a developer and working with a functions file, you can see both are loaded earlier in the process than the files you are working with. Meaning you can't modify processes (note you cannot overwrite functions) or variables that run later or have not been defined yet. Also theme developers may place hooks in their code to allow plugins to hook to or plugins might allow for other plugins to overwrite their variables. Now this may be confusing thus far, but hang in there. 

To understand `add_filter()` and `add_action()` we need to look at how the hooks are created in the first place. 


    $arga= 'hello';
    do_action('im_a_hook', $arga );

When you encounter the above in WordPress, it will call any functions attached to the hook `im_a_hook` (look up `$wp_filter` for information on the process). In your attached function `$arga` will be available for the attached function to work with. 

    add_action('im_a_hook', 'attached_function');

    function attached_function($arga){
         echo $arga;
    }

This opens up powerful new opportunities to modify variables at certain points of the load process. Remember we said earlier that templates are loaded after plugins/ themes? One common plugin is WooCommerce which creates screens later in the process, I'm not going to document how but an example of `do_action` can be found in the plugin.

    do_action( 'woocommerce_after_add_to_cart_button' );

Here we have a hook created that passes no variables back, but we can still have fun with it:

    add_action( 'woocommerce_after_add_to_cart_button', 'special_offer');

    function special_offer(){
        echo '<h1>Special Offer!</h1>;
    }

The above `add_action` will `echo` a heading of special offer where `do_action('woocommerce_after_add_to_cart_button')` is located which is when creating a WooCommerce screen. So we can use this hook to insert html. Other uses might include redirecting to a different screen altogether, etc.

Also multiple variables may be passed to the function. Try this in your themes functions. Note the last parameter we are setting to 3, because we want to work with the 3 available parameters. If we changed this to 2, only 2 would be returned and we would get a undefined error.

    add_action('custom_hook', 'attached_function', 10, 3);

    function attached_function($a,$b,$c){
        
        var_dump($a);
        var_dump($b);
        var_dump($c);
        
    }
        

    $arga = 1;
    $argb = 2;
    $argc = 3;

    do_action('custom_hook', $arga, $argb, $argc);
    exit;

There is another WP hook type called a filter. A filter is different from an action in its usage, an action can only receive variables, obviously these variables are within the functions scope (you should know what php scope is, if not google). Filters pass back the returned data, so you can use to modify variables. 


    $filter_me= apply_filters('im_a_filter', $variable_to_filter);

Where you see the above, you can modify the value of `$filter_me` as any data you return will be the value stored in the variable. So for example (note we are changing `$variable_to_filter` to `$filter_me` in the example):

    add_filter('im_a_filter', 'attached_function', 100);

    function attached_function($filter_me){
        
        $filter_me= 'ray';
        
        return $filter_me;
        
    }


    $filter_me = 'bob';
    $filter_me= apply_filters('im_a_filter', $filter_me);

The `$filter_me` variable will now contain *'ray'* rather than *'bob'*, we have set a priority of 100 so we are reasonably confident no one is changing the value after use (there can be multiple filters running on the same hook) So we can now change variables used later in the process if `apply_filters()` is present. 

You can also pass multiple parameters, but you can only change the value of one. You must also return a value, or else your variable will contain nothing. If you understand how you use php to assign values/arrays/objects to variables this will be obvious to you, e.g.:


    add_filter('im_a_filter', 'attached_function', 100, 3);

    function attached_function($filter_me, $arga, $argb){
        
        $filter_me= 'ray'.$arga.$argb;

        $arga= 'you fool';
        
        return $filter_me;
        
    }

    $filter_me = 'bob';
    
    $arga = ' middlename';
    $argb = ' surname';
    
    $filter_me= apply_filters('im_a_filter', $filter_me, $arga, $argb);

The `$filter_me` variable now contains *'ray middlename surname'*. But what about `$arga`? This still contains *'middlename'*, changing an `$arga` to *'you fool'* within our function has no effect on the defined value outside of its scope (there are ways, google globals etc.)



**add_action($hook_name, $function, $priority, $parameters)**

**add_filter($hook_name, $function, $priority, $parameters);**

## add_action - init
    add_action('init', 'process_post');

    function process_post(){
       if($_POST)
         var_dump($_POST);
    }

## add_action - init - anonymous function
    add_action('init' , function(){
        echo 'i did something';
    });

## add_action - init - within class object
     class sample{

         public function __construct(){
             add_action('init', array($this, 'samp') );
         }


         public function samp(){ // must be public!!
             echo 'i did something';
         }
     }
  
     new sample();   

## add_action - init - within static class
     class sample{

         public static function add_action_func(){
             //note __CLASS__ will also include any namespacing
             add_action('init', array(__CLASS__, 'samp') );
         }

         public static function samp(){
             echo 'i did something';
         }
         
     }

     sample::add_action_func();

