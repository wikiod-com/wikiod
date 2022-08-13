---
title: "Control Structures"
slug: "control-structures"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## if  else
The `if` statement in the example above allows to execute a code fragment, when the condition is met. When you want to execute a code fragment, when the condition is not met you extend the `if` with an `else`. 

    if ($a > $b) {
      echo "a is greater than b";
    } else {
      echo "a is NOT greater than b";
    }

[PHP Manual - Control Structures - Else][1]

**The ternary operator as shorthand syntax for if-else**

The [ternary operator][2] evaluates something based on a condition being true or not. It is a comparison operator and often used to express a simple if-else condition in a shorter form. It allows to quickly test a condition and often replaces a multi-line if statement, making your code more compact.

This is the example from above using a ternary expression and variable values: `$a=1; $b=2;`
 
    echo ($a > $b) ? "a is greater than b" : "a is NOT greater than b";

Outputs: `a is NOT greater than b`.


  [1]: http://php.net/manual/en/control-structures.else.php
  [2]: http://php.net/manual/de/language.operators.comparison.php#language.operators.comparison.ternary

## Alternative syntax for control structures
PHP provides an alternative syntax for some control structures: `if`, `while`, `for`, `foreach`, and `switch`. 

When compared to the normal syntax, the difference is, that the opening brace is replaced by a colon (`:`) and the closing brace is replaced by `endif;`, `endwhile;`, `endfor;`, `endforeach;`, or `endswitch;`, respectively. For individual examples, see the topic on [alternative syntax for control structures][1].

    if ($a == 42):
        echo "The answer to life, the universe and everything is 42.";
    endif;

Multiple `elseif` statements using short-syntax:

    if ($a == 5):
        echo "a equals 5";
    elseif ($a == 6):
        echo "a equals 6";
    else:
        echo "a is neither 5 nor 6";
    endif;

[PHP Manual - Control Structures - Alternative Syntax][2]


  [1]: https://www.wikiod.com/php/alternative-syntax-for-control-structures
  [2]: http://php.net/manual/en/control-structures.alternative-syntax.php

## while
`while` loop iterates through a block of code as long as a specified condition is true.

    $i = 1;
    while ($i < 10) {
        echo $i;
        $i++;
    }

Output: `123456789`

For detailed information, see [the Loops topic](https://www.wikiod.com/php/loops#while).


## do-while
`do-while` loop first executes a block of code once, in every case, then iterates through that block of code as long as a specified condition is true.


    $i = 0;
    do {
        $i++;
        echo $i;
    } while ($i < 10);

    Output: `12345678910`

For detailed information, see [the Loops topic](https://www.wikiod.com/php/loops#do...while).

## goto
The `goto` operator allows to jump to another section in the program.
It's available since PHP 5.3.

The goto instruction is a goto followed by the desired target label: `goto MyLabel;`.

The target of the jump is specified by a label followed by a colon: `MyLabel:`.

This example will print `Hello World!`:

    <?php
    goto MyLabel;
    echo 'This text will be skipped, because of the jump.';
     
    MyLabel:
    echo 'Hello World!';
    ?>

## declare
`declare` is used to set an execution directive for a block of code.

The following directives are recognized:
- [`ticks`][1]
- [`encoding`][2]
- [`strict_types`][3]

For instance, set ticks to 1:

    declare(ticks=1);

To enable strict type mode, the `declare` statement is used with the `strict_types` declaration:

    declare(strict_types=1);


  [1]: http://php.net/manual/en/control-structures.declare.php#control-structures.declare.ticks
  [2]: http://php.net/manual/en/control-structures.declare.php#control-structures.declare.encoding
  [3]: http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration.strict

## include & require
require
==

`require` is similar to `include`, except that it will produce a fatal `E_COMPILE_ERROR` level error on failure. When the `require` fails, it will halt the script. When the `include` fails, it will not halt the script and only emit `E_WARNING`.

    require 'file.php';

[PHP Manual - Control Structures - Require][1]

include
==

The `include` statement includes and evaluates a file.

>*./variables.php*

    $a = 'Hello World!';


>./main.php`

    include 'variables.php';
    echo $a;
    // Output: `Hello World!`

Be careful with this approach, since it is considered a [code smell][2], because the included file is altering amount and content of the defined variables in the given scope.

----

You can also `include` file, which returns a value. This is extremely useful for handling configuration arrays:

> configuration.php

    <?php 
    return [
        'dbname' => 'my db',
        'user' => 'admin',
        'pass' => 'password',
    ];
> main.php

    <?php
    $config = include 'configuration.php';

This approach will prevent the included file from polluting your current scope with changed or added variables.

[PHP Manual - Control Structures - Include][3]


  [1]: http://php.net/manual/en/function.require.php
  [2]: https://en.wikipedia.org/wiki/Code_smell


----------

**include & require** can also be used to assign values to a variable when returned something by file.

Example : 

include1.php file :

    <?php
        $a = "This is to be returned";
    
        return $a;
    ?>

index.php file : 

        $value = include 'include1.php';
       // Here, $value = "This is to be returned"

  [3]: http://php.net/manual/en/function.include.php

## return
The `return` statement returns the program control to the calling function.

 When `return` is called from within a function, the execution of the current function will end.

    function returnEndsFunctions()
    {
       echo 'This is executed';
       return;
       echo 'This is not executed.';
    }

When you run `returnEndsFunctions();` you'll get the output `This is executed`;

When `return` is called from within a function with and argument, the execution of the current function will end and the value of the argument will be returned to the calling function.

## for
`for` loops are typically used when you have a piece of code which you want to repeat a given number of times.

    for ($i = 1; $i < 10; $i++) {
        echo $i;
    }

Outputs: `123456789`

For detailed information, see [the Loops topic](https://www.wikiod.com/php/loops#for).



## foreach
`foreach` is a construct, which enables you to iterate over arrays and objects easily.

    $array = [1, 2, 3];
    foreach ($array as $value) {
        echo $value;
    }

Outputs: `123`.

To use `foreach` loop with an object, it has to implement [`Iterator`][1] interface.

When you iterate over associative arrays: 

    $array = ['color'=>'red']; 
    
    foreach($array as $key => $value){
        echo $key . ': ' . $value; 
    }

Outputs: `color: red`

For detailed information, see [the Loops topic](https://www.wikiod.com/php/loops#foreach).

  [1]: http://php.net/manual/en/class.iterator.php

## if elseif else
**elseif**

`elseif` combines `if` and `else`. The `if` statement is extended to execute a different statement in case the original `if` expression is not met. But, the alternative expression is only executed, when the `elseif` conditional expression is met.

The following code displays either "a is bigger than b", "a is equal to b" or "a is smaller than b":    

    if ($a > $b) {
        echo "a is bigger than b";
    } elseif ($a == $b) {
        echo "a is equal to b";
    } else {
        echo "a is smaller than b";
    }

----

**Several elseif statements**

You can use multiple elseif statements within the same if statement:

    if ($a == 1) {
        echo "a is One";
    } elseif ($a == 2) {
        echo "a is Two";
    } elseif ($a == 3) {
        echo "a is Three";
    } else {
        echo "a is not One, not Two nor Three";
    }

## if
The if construct allows for conditional execution of code fragments.

    if ($a > $b) {
      echo "a is bigger than b";
    }

[PHP Manual - Control Structures - If][1]


  [1]: http://php.net/manual/en/control-structures.if.php

## switch
The `switch` structure performs the same function as a series of `if` statements, but can do the job in fewer lines of code. The value to be tested, as defined in the `switch` statement, is compared for equality with the values in each of the `case` statements until a match is found and the code in that block is executed. If no matching `case` statement is found, the code in the `default` block is executed, if it exists.

Each block of code in a `case` or `default` statement should end with the `break` statement. This stops the execution of the `switch` structure and continues code execution immediately afterwards. If the `break` statement is omitted, the next `case` statement's code is executed, *even if there is no match*. This can cause unexpected code execution if the `break` statement is forgotten, but can also be useful where multiple `case` statements need to share the same code.

    switch ($colour) {
    case "red":
        echo "the colour is red";
        break;
    case "green":
    case "blue":
        echo "the colour is green or blue";
        break;
    case "yellow":
        echo "the colour is yellow";
        // note missing break, the next block will also be executed
    case "black":
        echo "the colour is black";
        break;
    default:
        echo "the colour is something else";
        break;
    }

In addition to testing fixed values, the construct can also be coerced to test dynamic statements by providing a boolean value to the `switch` statement and any expression to the `case` statement. Keep in mind the *first* matching value is used, so the following code will output "more than 100":

    $i = 1048;
    switch (true) {
    case ($i > 0):
        echo "more than 0";
        break;
    case ($i > 100):
        echo "more than 100";
        break;
    case ($i > 1000):
        echo "more than 1000";
        break;
    }

For possible issues with loose typing while using the `switch` construct, see [Switch Surprises][1]


  [1]: https://www.wikiod.com/php/type-juggling-and-non-strict-comparison-issues#Switch surprises

