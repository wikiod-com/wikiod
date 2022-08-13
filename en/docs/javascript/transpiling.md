---
title: "Transpiling"
slug: "transpiling"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

Transpiling is the process of interpreting certain programming languages and translating it to a specific target language. In this context, transpiling  will take [compile-to-JS languages](https://github.com/jashkenas/coffeescript/wiki/list-of-languages-that-compile-to-js) and translate them into the **target** language of Javascript.

Transpiling is the process of converting source code to source code, and this is a common activity in JavaScript development.

The features available in common JavaScript applications (Chrome, Firefox, NodeJS, etc.) often lag behind the latest ECMAScript specifications (ES6/ES2015, ES7/ES2016, etc.). Once a specification has been approved, it will most certainly be available natively in future versions of JavaScript applications.

Rather than waiting for new JavaScript releases, engineers can start writing code that will run natively in the future (future-proofing) by using a compiler to convert code written for newer specifications into code compatible with existing applications. Common transpilers include [Babel](https://babeljs.io/) and [Google Traceur](https://github.com/google/traceur-compiler).

Transpilers can also be used to convert from another language like TypeScript or CoffeeScript to regular, "vanilla" JavaScript. In this case, transpiling converts from one language to a different language.

## Introduction to Transpiling
# Examples

**ES6/ES2015 to ES5 (via [Babel](https://babeljs.io/))**:

This ES2015 syntax

    // ES2015 arrow function syntax 
    [1,2,3].map(n => n + 1); 
    
is interpreted and translated to this ES5 syntax: 

    // Conventional ES5 anonymous function syntax 
    [1,2,3].map(function(n) {   
        return n + 1; 
    });

**CoffeeScript to Javascript (via built-in CoffeeScript compiler)**:

This CoffeeScript

    # Existence:
    alert "I knew it!" if elvis?

is interpreted and translated to Javascript:

    if (typeof elvis !== "undefined" && elvis !== null) {
      alert("I knew it!");
    }


----------

**How do I transpile?**

Most compile-to-Javascript languages have a transpiler **built-in** (like in CoffeeScript or TypeScript). In this case, you may just need to enable the language's transpiler via config settings or a checkbox. Advanced settings can also be set in relation to the transpiler.

For **ES6/ES2016-to-ES5 transpiling**, the most prominent transpiler being used is [Babel](https://babeljs.io/).


----------

**Why should I transpile?**

The most cited benefits include:

 - The ability to use newer syntax reliably
 - Compatibility among most, if not all browsers
 - Usage of missing/not yet native features to Javascript via languages like CoffeeScript or TypeScript

## Start using ES6/7 with Babel
[Browser support for ES6][1] is growing, but to be sure your code will work on environments that dont fully support it, you can use [Babel][2], the ES6/7 to ES5 transpiler, [try it out!][3]
 
 If you would like to use ES6/7 in your projects without having to worry about compatibility, you can use [Node][4] and [Babel CLI][5]
 
 
 ----------
 
 Quick setup of a project with Babel for ES6/7 support
 ----------------------------------------------------------
 
 1. [Download][6] and install Node
 2. Go to a folder and create a project using your favourite command line tool
 
 
    ~ npm init
 3. Install Babel CLI
 
 
    ~ npm install --save-dev babel-cli
    ~ npm install --save-dev babel-preset-es2015
 4. Create a `scripts` folder to store your `.js` files, and then a `dist/scripts` folder where the transpiled fully compatible files will be stored.
 5. Create a `.babelrc` file in the root folder of your project, and write this on it
 
 

    {
        "presets": ["es2015"]
    }

 6. Edit your `package.json` file (created when you ran `npm init`) and add the `build` script to the `scripts` property:
 
 

    {
        ...
        "scripts": {
        ... ,
        "build": "babel scripts --out-dir dist/scripts"
        },
        ...
    }

 7. Enjoy [programming in ES6/7][7]
 8. Run the following to transpile all your files to ES5
 
 
    ~ npm run build
 
 
 
 For more complex projects you might want to take a look at [Gulp][8] or [Webpack][9]
 
 
 
 
 [1]: https://kangax.github.io/compat-table/es6/
 [2]: https://babeljs.io/
 [3]: https://babeljs.io/repl/
 [4]: https://nodejs.org/en/
 [5]: https://babeljs.io/docs/usage/cli/
 [6]: https://nodejs.org/en/download/
 [7]: https://babeljs.io/docs/learn-es2015/
 [8]: http://gulpjs.com/
 [9]: https://webpack.github.io/

