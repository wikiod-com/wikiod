---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Existential Operator
CoffeeScript's existential operator `?` check if the variable is **null** or **undefined**.

**1. Check for `null` or `undefined`.**

    alert "Hello CoffeeScript!" if myVar?
javascript equivalent:

    if (typeof myVar !== "undefined" && myVar !== null) {
      alert("Hello CoffeeScript!");
    }

**2. Safer conditional assignment**
 
You can also use this operator safer conditional assignment

    language = favoriteLanguage ? "coffeescript"

javascript equivalent:

    language = typeof favoriteLanguage !== "undefined" && favoriteLanguage !== null ? favoriteLanguage : "coffeescript";

**3. Safe chaining of methods**

Instead of chaining the methods with `.` chain them with `?.` to avoid raising the **TypeError**. 

    firstName = user?.profile?.firstname

javascript equivalent:

    firstName = typeof user !== "undefined" && user !== null ? (ref = user.profile) != null ? ref.firstname() : void 0 : void 0;

If all of the properties exist then you'll get the expected result if the chain is broken, **undefined** is returned


## Full list of default operators
| CoffeeScript | JavaScript |
| :------: | :------: |
| `is`, `==` | `===` |
| `isnt`, `!=` | `!==` |
| `not` | `!` |
| `and` | `&&` |
| `or` | <code>&#124;&#124;</code> |
| `true`, `yes`, `on` | `true` |
| `false`, `no`, `off` | `false` |
| `@`, `this` | `this` |
| `of` | `in` |
| `in` | *No equivalent* |
| `a ** b` | `Math.pow(a, b)` |
| `a // b` | `Math.floor(a / b)` |
| `a %% b` | `(a % b + b) % b` |

