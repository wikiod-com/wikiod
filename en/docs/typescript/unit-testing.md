---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9256
type: docs
toc: true
---

## tape
[tape][1] is minimalistic JavaScript testing framework, it outputs [TAP-compliant][2] markup.

To install `tape` using `npm` run command

    npm install --save-dev tape @types/tape

To use `tape` with Typescript you need to install `ts-node` as global package, to do this run command

    npm install -g ts-node

Now you are ready to write your first test

    //math.test.ts
    import * as test from "tape";
    
    test("Math test", (t) => {
        t.equal(4, 2 + 2);
        t.true(5 > 2 + 2);
    
        t.end();
    });

To execute test run command

    ts-node node_modules/tape/bin/tape math.test.ts

In output you should see

    TAP version 13
    # Math test
    ok 1 should be equal
    ok 2 should be truthy
    
    1..2
    # tests 2
    # pass  2
    
    # ok

Good job, you just ran your TypeScript test.

**Run multiple test files**

You can run multiple test files at once using path wildcards.
To execute all Typescript tests in `tests` directory run command

    ts-node node_modules/tape/bin/tape tests/**/*.ts

  [1]: https://github.com/substack/tape
  [2]: https://testanything.org/

## jest (ts-jest)
[jest][1] is painless JavaScript testing framework by Facebook, with [ts-jest][2] can be used to test TypeScript code.

To install jest using npm run command

    npm install --save-dev jest @types/jest ts-jest typescript

For ease of use install `jest` as global package

    npm install -g jest

To make `jest` work with TypeScript you need to add configuration to `package.json`

```
//package.json
{
...
"jest": {
    "transform": {
      ".(ts|tsx)": "<rootDir>/node_modules/ts-jest/preprocessor.js"
    },
    "testRegex": "(/__tests__/.*|\\.(test|spec))\\.(ts|tsx|js)$",
    "moduleFileExtensions": ["ts", "tsx", "js"]
  }
}
```

Now `jest` is ready. 
Assume we have sample fizz buz to test

```
//fizzBuzz.ts
export function fizzBuzz(n: number): string {
    let output = "";
    for (let i = 1; i <= n; i++) {
        if (i % 5 && i % 3) {
            output += i + ' ';
        }
        if (i % 3 === 0) {
            output += 'Fizz ';
        }
        if (i % 5 === 0) {
            output += 'Buzz ';
        }
    }
    return output;
}
```

Example test could look like

```
//FizzBuzz.test.ts
/// <reference types="jest" />

import {fizzBuzz} from "./fizzBuzz";
test("FizzBuzz test", () =>{
    expect(fizzBuzz(2)).toBe("1 2 ");
    expect(fizzBuzz(3)).toBe("1 2 Fizz ");
});
```
To execute test run

    jest

In output you should see

```
 PASS  ./fizzBuzz.test.ts
  ✓ FizzBuzz test (3ms)

Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        1.46s, estimated 2s
Ran all test suites.
```

# Code coverage

`jest` supports generation of code coverage reports.

To use code coverage with TypeScript you need to add another configuration line to `package.json`.

```
{
...
  "jest": {
  ...
    "testResultsProcessor": "<rootDir>/node_modules/ts-jest/coverageprocessor.js"
  }
}
```

To run tests with generation of coverage report run

```
jest --coverage
```

If used with our sample fizz buzz you should see

```
 PASS  ./fizzBuzz.test.ts
  ✓ FizzBuzz test (3ms)

-------------|----------|----------|----------|----------|----------------|
File         |  % Stmts | % Branch |  % Funcs |  % Lines |Uncovered Lines |
-------------|----------|----------|----------|----------|----------------|
All files    |    92.31 |     87.5 |      100 |    91.67 |                |
 fizzBuzz.ts |    92.31 |     87.5 |      100 |    91.67 |             13 |
-------------|----------|----------|----------|----------|----------------|
Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        1.857s
Ran all test suites.
```
`jest` also created folder `coverage` which contains coverage report in various formats, including user friendly html report in `coverage/lcov-report/index.html` 

[![jest html report][3]][3]


  [1]: https://facebook.github.io/jest/
  [2]: https://www.npmjs.com/package/ts-jest
  [3]: https://i.stack.imgur.com/PtNG7.png

## Alsatian
[Alsatian](https://github.com/alsatian-test/alsatian) is a unit testing framework written in TypeScript. It allows for usage of Test Cases, and outputs [TAP-compliant](https://testanything.org/) markup.

To use it, install it from `npm`:

    npm install alsatian --save-dev

Then set up a test file:

    import { Expect, Test, TestCase } from "alsatian";
    import { SomeModule } from "../src/some-module";    

    export SomeModuleTests {

        @Test()
        public statusShouldBeTrueByDefault() {
            let instance = new SomeModule();
            
            Expect(instance.status).toBe(true);
        }
        
        @Test("Name should be null by default")
        public nameShouldBeNullByDefault() {
            let instance = new SomeModule();
            
            Expect(instance.name).toBe(null);
        }
        
        @TestCase("first name")
        @TestCase("apples")
        public shouldSetNameCorrectly(name: string) {
            let instance = new SomeModule();
            
            instance.setName(name);
            
            Expect(instance.name).toBe(name);
        }
        
    }

For a full documentation, see [alsatian's GitHub repo](https://github.com/alsatian-test/alsatian).

## chai-immutable plugin

1. Install from npm chai, chai-immutable, and ts-node

       npm install --save-dev chai chai-immutable ts-node


2. Install types for mocha and chai

       npm install --save-dev @types/mocha @types/chai

3. Write simple test file:

         import {List, Set} from 'immutable';
         import * as chai from 'chai';
         import * as chaiImmutable from 'chai-immutable';

         chai.use(chaiImmutable);

         describe('chai immutable example', () => {
           it('example', () => {
             expect(Set.of(1,2,3)).to.not.be.empty;

             expect(Set.of(1,2,3)).to.include(2);
             expect(Set.of(1,2,3)).to.include(5);
           })
         })

4. Run it in the console:

       mocha --compilers ts:ts-node/register,tsx:ts-node/register 'test/**/*.spec.@(ts|tsx)'


