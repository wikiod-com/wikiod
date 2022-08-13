---
title: "Basic template syntax"
slug: "basic-template-syntax"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Explanation of basic template syntax constructs

## Accessing variables
In Twig templates variables can be accessed using double curly braces notation `{{ variableName }}`.

Basic example of greeting user 

    <!DOCTYPE html>
    <html>
      <body>
        <span>Hello {{ name }}</span>
      </body>
    </html>

## Accessing array elements ##

Twig as a parameter can receive array. To access a specific element of array you can use regular php array access bracket notation `{{ array[key] }}`.

Previous example modified to use array as a parameter

    <!DOCTYPE html>
    <html>
      <body>
        <span>Hello {{ user['name'] }}</span>
      </body>
    </html>

## Accessing object properties ##
Objects also can be passed as a parameter to template. 'Dot' (.) notation is used to access specific object properties `{{ object.propertyName }}`.

Same example with object as a parameter

    <!DOCTYPE html>
    <html>
      <body>
        <span>Hello {{ user.name }}</span>
      </body>
    </html>

## Filters
Variables can be modified using filters. To apply filter to variable follow variable name with pipe `|` and filter name:
 

    {{ variable|filterName }}

For example to display variable value in uppercase use construct.


    {{ variable|upper }}

Filters can be parametrized. Filter parameters are passed inside parentheses as a comma(,) separated list :

    {{ variable|filterName(param1, param2, ...) }}

To round number to given precision we can use filter `round`, it accepts up to 2 parameters. First one specifies precision (default: 0), second one rounding method (default: common).

To round number to 1 decimal place using common method you can use 
`{{ number|round(1, 'common') }}` or `{{ number|round(1) }}` as common is default method.

Filters can also be used on embedded objects & array variables:

    {{ array['key'] | upper }} {{ object.text | upper }}

Filters can also be concatenated:

    {% set array = "3,1,2"|split(',') %}
    
    {{ array | sort | first }}

List of basic filters is available [here][1]


  [1]: http://twig.sensiolabs.org/doc/filters/index.html


## Conditional blocks
Parts of template can be displayed conditionally. `If` statement is used for this purpose. It's similar to `if` statement in programing languages.
Contents of block are executed/displayed if an expression evaluates to `true`.

    {% if enabled == false %}
        Disabled
    {% endif %}

*Disabled* will be displayed only when `enabled` will be equal `false`.

Multiple branches can be created using `elseif` and `else`.

    {% if temperature < 10 %}
        It's cold
    {% elseif temperature < 18 %}
        It's chilly
    {% elseif temperature < 24 %}
        It's warm
    {% elseif temperature < 32 %}
        It's hot
    {% else %}
        It's very hot
    {% endif %}

## For Loop
For loops can be really useful in TWIG, allowing the creation of data-dynamic web pages. 

Say we create a simple array of numbers:

    {% set array = "3,1,2" %}

We can then iterate over the array and print out whatever we want. Any data within the array block will be outputted in relation to the amount of data within the array. This example would print out three h1 elements with the array data concatenated.

    {% for current in array %}
        <h1>This is number {{ current }} in the array </h1>
    {% endear %}

Note that `{{ current }}` was used to access the data and not a version of `array`

See this working: https://twigfiddle.com/mxwkea/2

A further example of this could be using objects. For example, say we have an Entity object with the field 'name' along with its relevant *getters and setters*. If a number of these entities are also stored within the Array, they can be accessed like any other Objects within TWIG:

    {% for currentObject in ArrayOfObjects %}
        {{ currentObject.name }}
    {% endfor %}
See this working with JSON data: https://twigfiddle.com/mxwkea

## Ternary Operator (Shorthand If-Then-Else) & Null-Coalescing Operator
# The ternary operator (`?:`)

Support for the extended ternary operator was added in **Twig 1.12.0**.

    {{ foo ? 'yes' : 'no' }}
Evaluates:
>if `foo` echo `yes` else echo `no`
---
    {{ foo ?: 'no' }}
or

    {{ foo ? foo : 'no' }}
Evaluates:
> if `foo` echo it, else echo `no`
---
    {{ foo ? 'yes' }}
or

    {{ foo ? 'yes' : '' }}
Evaluates:
> if `foo` echo `yes` else echo nothing
---
# The null-coalescing operator (`??:`)

    {{ foo ?? 'no' }}
Evaluates:
>Returns the value of `foo` if it **is defined** and **not null**, `no` otherwise


## Whitespace handling
To remove whitespace (spaces, tabs, newlines…) between HTML tags use `spaceless` tag:

    {% spaceless %}

        <div>
            <span>foo bar </span>
        </div>
    {% endspaceless %}
    {# produces output <div><strong>foo bar </strong></div> #}

If you need to remove whitespace on a per tag level, use *whitespace control modifier* i.e. hyphen (-). Using it, you can trim leading and or trailing whitespace:

    {% set value = 'foo bar' %}

    <span>  {{- value }}  </span> 
    {# produces '<span>foo bar  </span>' #}

    <span>  {{ value -}}  </span> 
    {# produces '<span>  foo bar</span>' #}

    <span>  {{- value -}}  </span> 
    {# produces '<span>foo bar</span>' #}

    <span {%- if true  %} class="foo"{% endif %}>
    {# produces '<span class="foo">' #}

    <span {%- if false %} class="foo"{% endif %}>
    {# produces '<span>' #}

