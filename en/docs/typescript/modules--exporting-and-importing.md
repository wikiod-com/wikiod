---
title: "Modules - exporting and importing"
slug: "modules---exporting-and-importing"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Re-export
Typescript allow to re-export declarations.

```
//Operator.ts
interface Operator {
    eval(a: number, b: number): number;
}
export default Operator;
```

```
//Add.ts
import Operator from "./Operator";
export class Add implements Operator {
    eval(a: number, b: number): number {
        return a + b;
    }
}
```

```
//Mul.ts
import Operator from "./Operator";
export class Mul implements Operator {
    eval(a: number, b: number): number {
        return a * b;
    }
}
```

You can bundle all operations in single library

```
//Operators.ts
import {Add} from "./Add";
import {Mul} from "./Mul";

export {Add, Mul};
```

**Named declarations** can be re-exported using shorter syntax

```
//NamedOperators.ts
export {Add} from "./Add";
export {Mul} from "./Mul";
```

**Default exports** can also be exported, but no short syntax is available. Remember, only one default export per module is possible.

```
//Calculator.ts
export {Add} from "./Add";
export {Mul} from "./Mul";
import Operator from "./Operator";

export default Operator;
```

Possible is re-export of **bundled import**
```
//RepackedCalculator.ts
export * from "./Operators";
```

When re-exporting bundle, declarations may be overridden when declared explicitly.
```
//FixedCalculator.ts
export * from "./Calculator"
import Operator from "./Calculator";
export class Add implements Operator {
    eval(a: number, b: number): number {
        return 42;
    }
}
```
Usage example
```
//run.ts
import {Add, Mul} from "./FixedCalculator";

const add = new Add();
const mul = new Mul();

console.log(add.eval(1, 1)); // 42
console.log(mul.eval(3, 4)); // 12
```

## Hello world module
```typescript
//hello.ts
export function hello(name: string){
    console.log(`Hello ${name}!`);
} 
function helloES(name: string){
    console.log(`Hola ${name}!`);
}
export {helloES};
export default hello;
```

**Load using directory index**

If directory contains file named `index.ts` it can be loaded using only directory name (for `index.ts` filename is optional).

```typescript
//welcome/index.ts
export function welcome(name: string){
    console.log(`Welcome ${name}!`);
}
```

**Example usage of defined modules**

```typescript
import {hello, helloES} from "./hello";  // load specified elements
import defaultHello from "./hello";      // load default export into name defaultHello
import * as Bundle from "./hello";       // load all exports as Bundle
import {welcome} from "./welcome";       // note index.ts is omitted

hello("World");                          // Hello World!
helloES("Mundo");                        // Hola Mundo!
defaultHello("World");                   // Hello World!

Bundle.hello("World");                   // Hello World!
Bundle.helloES("Mundo");                 // Hola Mundo!

welcome("Human");                        // Welcome Human!
```

## Exporting/Importing declarations
Any declaration (variable, const, function, class, etc.) can be exported from module to be imported in other module.

Typescript offer two export types: named and default.

**Named export**

```typescript
// adams.ts
export function hello(name: string){
    console.log(`Hello ${name}!`);
}
export const answerToLifeTheUniverseAndEverything = 42;
export const unused = 0;
```

When importing named exports, you can specify which elements you want to import.
```typescript
import {hello, answerToLifeTheUniverseAndEverything} from "./adams";
hello(answerToLifeTheUniverseAndEverything);   // Hello 42!
```

**Default export**

Each module can have one default export

```typescript
// dent.ts
const defaultValue = 54;
export default defaultValue;
```
which can be imported using

```typescript
import dentValue from "./dent";
console.log(dentValue);        // 54
```

**Bundled import**

Typescript offers method to import whole module into variable

```typescript
// adams.ts
export function hello(name: string){
    console.log(`Hello ${name}!`);
}
export const answerToLifeTheUniverseAndEverything = 42;
```

```typescript
import * as Bundle from "./adams";
Bundle.hello(Bundle.answerToLifeTheUniverseAndEverything);  // Hello 42!
console.log(Bundle.unused);                                 // 0
```

