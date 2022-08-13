---
title: "Extending twig"
slug: "extending-twig"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Twig already has some built-in [filters](http://twig.sensiolabs.org/doc/filters/index.html) and [functions](http://twig.sensiolabs.org/doc/functions/index.html), but what if the built-in features are lacking or you have to access some default `PHP` functions in a template

## Creating a Twig_Extension
You can group all your custom functions/filters/tests/... inside a custom `Twig_Extension` class:

*ProjectTwigExtension* 

    class ProjectTwigExtension extends Twig_Extension {
    
        public function getFunctions() {
            return array(
                new Twig_SimpleFunction('twig_function_name', array($this, 'getTwigFunctionName')),
                new Twig_SimpleFunction('twig_function_foo', array($this, 'getTwigFunctionFoo')),            
            );



        }
        public function getFilters() {
            return array(
                new Twig_SimpleFilter('twig_filter_name' , array($this, 'getTwigFilterName')),
                new Twig_SimpleFilter('twig_filter_foo' , array($this, 'getTwigFilterFoo')),
            );
        }
        
        public function getName() {
            return 'ProjectTwigExtension';
        }        
    }



*Register extension in twig*

    $twig = new Twig_Environment($loader);
    $twig->addExtension(new ProjectTwigExtension());

More options can be found on the [official docs](http://twig.sensiolabs.org/doc/advanced.html#creating-an-extension)

## Simple Date of Birth to age filter
How to ...

1 - use twig extension class that extends

    use \Twig_Extension

    class dobToAge extends \Twig_Extension {

2 - Add the appropriate filter by overriding getFilters() method

     public function getFilters() {
            return array(
                'age' => new \Twig_Filter_Method($this, 'getAge'),
            );
     }

3 - Add some logic to get the age of a given Date of Birth

     public function getAge($date) 
         {
            if (!$date instanceof \DateTime) {
            // turn $date into a valid \DateTime object or let return
            return null;
             }

         $referenceDate = date('01-01-Y');
         $referenceDateTimeObject = new \DateTime($referenceDate);
         $diff = $referenceDateTimeObject->diff($date);
         return $diff->y;
        }
    }

Then, call your filter as follow,

    {{ yourDateOfBirthInstance | age }}

## Adding custom filters/functions
Here are some example on how to add new filters/functions to `twig`,<br />the synax for adding `Twig_Function`s are the same as the `Twig_Filter` ones, just change the keywords accordingly

    <?php
       $twig = new Twig_Environment($loader);

       /* You can chain a global function */
       $twig->addFilter(new Twig_SimpleFilter('floor', 'floor'));
       
       /* You can specify a custom function */
       $twig->addFilter(new Twig_SimpleFilter('money', function($value, $currency, $prefix = false, $decimals = 2, $dec_point = "." , $thousands_sep = ",") {
           $value = number_format($value, $decimals, $dec_point, $thousands_sep);
           if ($prefix) return $currency.' '.$value;
           return $value.' '.$prefix;
       });

       /* You can chain an object's method */
       $twig->addFilter(new Twig_SimpleFilter('foo_bar', array($foo, 'bar')));

