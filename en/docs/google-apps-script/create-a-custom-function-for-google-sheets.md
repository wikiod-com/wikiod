---
title: "Create a custom function for Google Sheets"
slug: "create-a-custom-function-for-google-sheets"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

A custom function in google docs is tied to a specific document (and thus can only be used in that document).

It must therefore be created with that document's scrip edit (Tools -> Script Editor). Once saved, it can then be used like any other regular spreadsheet formula.

## Standard gravity custom constant
This function return the standart gravity constant in the specified acceleration units (1 for cm/s², 2 for ft/s², 3 for m/s²)

<pre>
/**
 * Returns the standard gravity constant in the specified acceleration units
 * Values taken from https://en.wikipedia.org/wiki/Standard_gravity on July 24, 2016.
 *
 * @param  {number}  input  1 for cm/s², 2 for ft/s², 3 for m/s²
 *
 * @customfunction
 */
function sg(units_key) {
  var value;
  switch(units_key) {
    case 1:
      value = 980.665;
      break;
    case 2:
      value = 32.1740;
      break;
    case 3:
      value = 9.80665;
      break;
    default:
      throw new Error('Must to specify 1, 2 or 3');
  }
  return value;
}
</pre>

To use the function, it needs to be bound to a spreadsheet using the script editor (Tools -> Script editor...). Once the function is added, it can be used like any other google sheets function by calling the function in a cell's formula.

Note how the function shows up in autocomplete when typed into a formula. This is due to the multi-line comment above the function declaration which is used to describe what the function does similar to JSDoc and Javadoc. To have the formula show up in autocomplete, the @customfunction tag must be specified in the comment.

## Basic Example
To avoid unsightly `#DIV/0` errors in a spreadsheet, a custom function can be used.

    /**
     * Divides n by d unless d is zero, in which case, it returns
     * the given symbol.
     *
     * @param  {n}  number The numerator
     * @param  {d}  number The divisor
     * @param  {symbol}  string The symbol to display if `d == 0`
     * @return {number or string} The result of division or the given symbol
     *
     * @customfunction
     */
    function zeroSafeDivide(n, d, symbol) {  
      if (d == 0)
        return symbol;
      else
        return n / d;
    }

To use the function, it needs to be bound to a spreadsheet using the script editor (**Tools -> Script editor...**). Once the function is added, it can be used like any other google sheets function by calling the function in a cell's formula.

[![enter image description here][1]][1]

Note how the function shows up in autocomplete when typed into a formula. This is due to the multi-line comment above the function declaration which is used to describe what the function does similar to JSDoc and Javadoc. To have the formula show up in autocomplete, the `@customfunction` tag must be specified in the comment.


  [1]: https://i.stack.imgur.com/pWSFhm.png

