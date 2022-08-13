---
title: "Operator map  select"
slug: "operator-map--select"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Syntax
- **Rx.Observable.prototype.map(selector, [thisArg])**
- **Rx.Observable.prototype.select(selector, [thisArg])**

## Parameters
| Parameter, Type | Details |
| --- | --- | --- |
| `selector`, `Function` or `Object` | Transform function to apply to each source element or an element to yield. If selector is a function, it is called with the following information: 1. the value of the element, 2. the index of the element, 3. the Observable object being subscribed. |
| `[thisArg]`, `Any` | Object to use as `this` when executing the predicate. |

`map` and `select` are aliases.

They produce an observable sequence emitting one element every time the source observable emits an element.

If `selector` is not a function, its value is emitted for each source element.

If `selector` is a function, the emitted element is the result of running `selector` on the source element, and can possibly use its position.

## Using a transform function
    const source = Rx.Observable.range(1, 3)
      .map(x => x * x);
    
    const subscription = source.subscribe(
      x => console.log(`Next: ${x}`),
      err => console.log(`Error: ${err}`),
      () => console.log(`Completed`)
    );
    
    // => Next: 1
    // => Next: 4
    // => Next: 9
    // => Completed

## Using an element to yield
    const md = Rx.Observable.fromEvent(document, 'mousedown').map(true);
    // `md` will emit `true` whenever the mouse is pressed
    const mu = Rx.Observable.fromEvent(document, 'mouseup').map(false);
    // `mu` will emit `false` whenever the mouse is depressed

## Using a transform function and the element index
    const source = Rx.Observable.range(1, 3)
      .map((x, idx, obs) => `Element ${x} was at position ${idx}`);
    
    const subscription = source.subscribe(
      x => console.log(`Next: ${x}`),
      err => console.log(`Error: ${err}`),
      () => console.log(`Completed`)
    );
    
    // => Next: Element 1 was at position 0
    // => Next: Element 2 was at position 1
    // => Next: Element 3 was at position 2
    // => Completed

