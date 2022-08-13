---
title: "Templating"
slug: "templating"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Creating A Basic Template
In Aurelia all HTML templates are defined inside of opening and closing `<template></template>` tags. All of your HTML and Aurelia specific logic goes inside of these template tags and cannot exist outside of them.

```
<template>

</template>
```

## Working With Loops Using "repeat.for"
Looping over an iterable defined inside of your viewmodel or passed through as a bindable (if a Custom Attribute or Custom Element) can be done like so.

**An array of string values**

```
export class MyViewModel {
    myIterable = ['String 1', 'String 2', 'String 3', 'String 4'];
}
```

```
<template>
    <div repeat.for="item of myIterable">${item}</div>
</template>
```

**An array of objects**

```
export class MyViewModel {
    myIterable = [
        {name: "John Citizen", age: 42},
        {name: "Maxwell Smart", age: 75},
        {name: "Gary TwoShoes", age: 51}
    ];
}
```

```
<template>
    <div repeat.for="item of myIterable">
        <strong>Name:</strong> ${item.name}<br>
        <strong>Age:</strong> ${item.age}
    </div>
</template>
```

**A Map**

```
export class MyViewModel {
    myIterable = null;

    constructor() {
        this.myIterable = new Map();    
        this.myIterable.set(0, 'My Value');
        this.myIterable.set(1, 'Something Different');
        this.myIterable.set(2, 'Another String #32837');
    }
}
```

```
<template>
    <div repeat.for="[id, item] of myIterable">
        ${id}<br>
        ${item}
    </div>
</template>
```

**A numeric loop**

```
<template>
    <div repeat.for="i of 10">${i}</div>
</template>
```


