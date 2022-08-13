---
title: "Why and when to use TypeScript"
slug: "why-and-when-to-use-typescript"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

If you find the arguments for type systems persuasive in general, then you'll be happy with TypeScript.

It brings many of the advantages of type system (safety, readability, improved tooling) to the JavaScript ecosystem. It also suffers from some of the drawbacks of type systems (added complexity and incompleteness).

The merits of typed vs. untyped languages have been debated for decades. Arguments for static types include:

1. Safety: type systems allow many errors to be caught early, without running the code. TypeScript can be [configured to allow fewer programming errors][7]
1. Readability: explicit types make code easier for humans to understand. As Fred Brooks [wrote][1], "Show me your flowcharts and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won’t usually need your flowcharts; they’ll be obvious."
1. Tooling: type systems make code easier for computers to understand. This allows tools like IDEs and linters to be more powerful.
1. Performance: type systems make code run faster by reducing the need for runtime type checking.

Because [TypeScript's output is independent of its types][2], TypeScript has no impact on performance. The argument for using TypeScript rests on the other three advantages.

Arguments against type systems include:

1. Added complexity: type systems can be more complex than the language runtime that they describe. Higher order functions can be easy to implement correctly but [difficult to type][3]. Dealing with type definitions creates additional barriers to using external libraries.
1. Added verbosity: type annotations can add boilerplate to your code, making the underlying logic harder to follow.
1. Slower iteration: by introducing a build step, TypeScript slows down the edit/save/reload cycle.
1. Incompleteness: A type system cannot be both sound and complete. There are correct programs which TypeScript does not allow. And programs which TypeScript accepts can still contain bugs. A type system doesn't alleviate the need for testing. If you use TypeScript, you may have to wait longer to use new ECMAScript language features.

TypeScript offers some ways to address all of these issues:

1. Added complexity. If typing part of a program is too difficult, TypeScript can be largely disabled using an opaque `any` type. The same is true for external modules.
1. Added verbosity. This can partially be addressed through type aliases and TypeScript's ability to infer types. Writing clear code is as much an art as a science: remove too many type annotations and the code may no longer be clear to human readers.
1. Slower iteration: A build step is relatively common in modern JS development and TypeScript already [integrates with most build tools][6]. And if TypeScript catches an error early, it can save you an entire iteration cycle!
1. Incompleteness. While this problem cannot be completely solved, TypeScript has been able to capture more and more expressive JavaScript patterns over time. Recent examples include the addition of [mapped types in TypeScript 2.1][4] and [mixins in 2.2][5].

The arguments for and against type systems in general apply equally well to TypeScript. Using TypeScript increases the overhead to starting a new project. But over time, as the project increases in size and gains more contributors, the hope is that the pros of using it (safety, readability, tooling) become stronger and outweigh the cons. This is reflected in TypeScript's motto: "JavaScript that scales."

[1]: https://en.wikiquote.org/wiki/Fred_Brooks#The_Mythical_Man-Month:_Essays_on_Software_Engineering_.281975.2C_1995.29
[2]: http://neugierig.org/software/blog/2016/04/typescript-types.html
[3]: https://medium.com/javascript-scene/you-might-not-need-typescript-or-static-types-aa7cb670a77b#.sfoweuenw
[4]: https://medium.com/@danvdk/a-typed-pluck-exploring-typescript-2-1s-mapped-types-c15f72bf4ca8
[5]: https://blogs.msdn.microsoft.com/typescript/2017/02/02/announcing-typescript-2-2-rc/
[6]: https://www.wikiod.com/typescript/integrating-with-build-tools
[7]: https://www.wikiod.com/typescript/tsconfigjson#Configuration for fewer programming errors

## Safety
TypeScript catches type errors early through static analysis:

    function double(x: number): number {
      return 2 * x;
    }
    double('2');
    //     ~~~ Argument of type '"2"' is not assignable to parameter of type 'number'.

## Readability
TypeScript enables editors to provide contextual documentation:

[![Contextual autocomplete for string slice method][1]][1]

You'll never forget whether `String.prototype.slice` takes `(start, stop)` or `(start, length)` again!

  [1]: https://i.stack.imgur.com/olNJH.png

## Tooling
TypeScript allows editors to perform automated refactors which are aware of the rules of the languages.

[![A symbol being renamed, but only in its own scope][1]][1]

Here, for instance, Visual Studio Code is able to rename references to the inner `foo` without altering the outer `foo`. This would be difficult to do with a simple find/replace.

  [1]: https://i.stack.imgur.com/N1C0T.gif

