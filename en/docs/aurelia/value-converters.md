---
title: "Value Converters"
slug: "value-converters"
draft: false
images: []
weight: 9770
type: docs
toc: true
---

This section provides an overview of Value Converters in Aurelia. It should detail not only how to create a value converter, but also why you might want to use them and many examples of basic tasks accomplished through the use of a Value Converter.

Value converters can be chained and used alongside other binding features in Aurelia such as Binding Behaviors.

## Creating A Basic Value Converter
While Value Converters can be comprised of either a `toView` or `fromView` method, in the below example we will be creating a basic Value Converter which just uses the `toView` method which accepts the value being sent to the view as the first argument.

**to-uppercase.js**

```
export class ToUppercaseValueConverter {
    toView(value) {
        return value.toUpperCase();
    }
}
```

**Using it:**

```
export class MyViewModel {
    stringVal = 'this is my test string';
}
```

```
<template>
    <require from="./to-uppercase"></require>

    <h1 textContent.bind="stringVal | toUppercase"></h1>
</template>
```

The text value of our heading one element should be `THIS IS MY TEST STRING` this is because the `toView` method which accepts the value from the view and specifies that the view should get our new value which are are using `String.prototype.toUpperCase()`

The class name is in this case `ToUppercaseValueConverter`, where `ValueConverter` tells aurelia what it is (There is also a method with Anotations, but I didn't find an example on the internet). So the `ValueConverter` is necessary in the class name, but by calling the converter, this isn't necessary anymore, therefor you need to call the converter only with `toUppercase`in the html template.

## Chaining Value Converters
A Value Converter can be used alongside other value converters and you can infinitely chain them using the `|` pipe separator.

    ${myString | toUppercase | removeCharacters:'&,%,-,+' | limitTo:25}

The above theoretical example firstly applies `toUppercase` which capitalizes our string. Then it applies the `removeCharacters` Value Converter which allows us to remove specific characters from our string and lastly we limit the length of the string to 25 characters using `limitTo`.

**Note:** the above Value Converters do not actually exist. They are purely for example purposes only.

## Creating A Bi-directional Value Converter
A bi-directional value converter utilizes two methods in your Value Converter class: `toView` and `fromView` these methods are aptly named to signify which direction the data is flowing.

In our example we will be creating a prepend Value Converter which will make sure that an amount entered in our app has a dollar sign infront of it.

**prepend.js**

```
export class PrependValueConverter {
  
    /**
     * This is the value being sent back to the view
     *
     */
    toView(value) {
        return `$${value}`;
    }

    /**
     * Validate the user entered value
     * and round it to two decimal places
     *
     */
    fromView(value) {
        return parseFloat(value.toString().replace('$', '')).toFixed(2);
    }
}
```

**Using it:**

```
export class MyViewModel {
    amount = '';
}
```

```
<template>
    <require from="./prepend"></require>

    <h1>Current amount: ${amount}</h1>

    <label>Enter amount:</label>
    <input type="text" id="amount" value.bind="amount | prepend & debounce:500">
</template>
```

Worth noting is that we are using a binding behaviour to limit the rate of which our value is updated or it will be updated every time you type and not be the intended behaviour.

