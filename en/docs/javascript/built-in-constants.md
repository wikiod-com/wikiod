---
title: "Built-in Constants"
slug: "built-in-constants"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## null
`null` is used for representing the intentional absence of an object value and is a primitive value. Unlike `undefined`, it is not a property of the global object.

It is equal to `undefined` but not identical to it.

    null == undefined; // true
    null === undefined; // false

----------

**CAREFUL**: The `typeof` `null` is `'object'`.

    typeof null; // 'object';

To properly check if a value is `null`, compare it with the [strict equality operator][1]

    var a = null;

    a === null; // true


  [1]: https://www.wikiod.com/javascript/comparison-operations

## Testing for NaN using isNaN()


## NaN
[`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) stands for "Not a Number." When a mathematical function or operation in JavaScript cannot return a specific number, it returns the value `NaN` instead.

It is a property of the global object, and a reference to [`Number.NaN`][1]

    window.hasOwnProperty('NaN'); // true
    NaN; // NaN

Perhaps confusingly, `NaN` is still considered a number.

    typeof NaN; // 'number'

Don't check for `NaN` using the equality operator. See [`isNaN`](https://www.wikiod.com/javascript/built-in-constants#Testing for NaN using isNaN()) instead.

    NaN == NaN  // false
    NaN === NaN // false

  [1]: https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Number/NaN

## undefined and null
At first glance it may appear that `null` and `undefined` are basically the same, however there are subtle but important differences.

`undefined` is the absence of a value in the compiler, because where it should be a value, there hasn't been put one, like the case of an unassigned variable.

- `undefined` is a global value that represents the absence of an assigned value.
  - `typeof undefined === 'undefined'`
- `null` is an object that indicates that a variable has been explicitly assigned "no value".
  - `typeof null === 'object'`

Setting a variable to `undefined` means the variable effectively does not exist. Some processes, such as JSON serialization, may strip `undefined` properties from objects. In contrast, `null` properties indicate will be preserved so you can explicitly convey the concept of an "empty" property.

The following evaluate to `undefined`:

 - A variable when it is declared but not assigned a value (i.e. defined)
   -  ```
      let foo;
      console.log('is undefined?', foo === undefined);
      // is undefined? true
      ```
 - Accessing the value of a property that doesn't exist
   -  ```
      let foo = { a: 'a' };
      console.log('is undefined?', foo.b === undefined);
      // is undefined? true
      ```
 - The return value of a function that doesn't return a value
   -  ```
      function foo() { return; }
      console.log('is undefined?', foo() === undefined);
      // is undefined? true
      ```
 - The value of a function argument that is declared but has been omitted from the function call
   -  ```
      function foo(param) { 
        console.log('is undefined?', param === undefined);
      }
      foo('a');
      foo();
      // is undefined? false
      // is undefined? true
      ```

----------


`undefined` is also a property of the global `window` object.

    // Only in browsers
    console.log(window.undefined); // undefined
    window.hasOwnProperty('undefined'); // true    

<!-- if version [lt 5] -->
Before ECMAScript 5 you could actually change the value of the `window.undefined` property to any other value potentially breaking everything.
<!-- end version if -->


## Infinity and -Infinity
    1 / 0; // Infinity
    // Wait! WHAAAT?

`Infinity` is a property of the global object (therefore a global variable) that represents mathematical infinity. It is a reference to `Number.POSITIVE_INFINITY`

It is greater than any other value, and you can get it by dividing by 0 or by evaluating the expression of a number that's so big that overflows. This actually means there is no division by 0 errors in JavaScript, there is Infinity!

There is also `-Infinity`  which is mathematical negative infinity, and it's lower than any other value.

To get `-Infinity` you negate `Infinity`, or get a reference to it in `Number.NEGATIVE_INFINITY`.

    - (Infinity); // -Infinity

Now let's have some fun with examples:

    Infinity > 123192310293; // true
    -Infinity < -123192310293; // true
    1 / 0; // Infinity
    Math.pow(123123123, 9123192391023); // Infinity
    Number.MAX_VALUE * 2; // Infinity
    23 / Infinity; // 0
    -Infinity; // -Infinity
    -Infinity === Number.NEGATIVE_INFINITY; // true
    -0; // -0 , yes there is a negative 0 in the language
    0 === -0; // true
    1 / -0; // -Infinity
    1 / 0 === 1 / -0; // false
    Infinity + Infinity; // Infinity

    var a = 0, b = -0;

    a === b; // true
    1 / a === 1 / b; // false

    // Try your own!


## Operations that return NaN


## Math library functions that return NaN


## Number constants
The `Number` constructor has some built in constants that can be useful

```javascript
Number.MAX_VALUE;          // 1.7976931348623157e+308
Number.MAX_SAFE_INTEGER;   // 9007199254740991

Number.MIN_VALUE;          // 5e-324
Number.MIN_SAFE_INTEGER;   // -9007199254740991

Number.EPSILON;            // 0.0000000000000002220446049250313

Number.POSITIVE_INFINITY;  // Infinity
Number.NEGATIVE_INFINITY;  // -Infinity

Number.NaN;                // NaN
```

In many cases the various operators in Javascript will break with values outside the range of (`Number.MIN_SAFE_INTEGER`, `Number.MAX_SAFE_INTEGER`)

Note that `Number.EPSILON` represents the different between one and the smallest `Number` greater than one, and thus the smallest possible difference between two different `Number` values. One reason to use this is due to the nature of how numbers are stored by JavaScript see [Check the equality of two numbers](https://www.wikiod.com/javascript)

