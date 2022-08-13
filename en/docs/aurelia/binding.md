---
title: "Binding"
slug: "binding"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Binding To Checkboxes
**Basic Checkboxes**

```
export class MyViewModel {
    favoriteColors = [];
    colors = ['Red', 'Yellow', 'Pink', 'Green', 'Purple', 'Orange', 'Blue'];
}
```

```
<template>
  <label repeat.for="color of colors">
    <input type="checkbox" value.bind="color" checked.bind="favoriteColors" />
    ${color}
  </label>
  
  <p>Favourite colors:</p>
  <ul if.bind="favoriteColors">
    <li repeat.for="color of favoriteColors">${color}</li>
  </ul>
</template>
```

**Checkboxes With Object Arrays**

```
export class MyViewModel {
    people = [];
    selectedPeople = [];
    
    constructor() {
        this.people = [
            {name: 'John Michaels'},
            {name: 'Gary Stevens'},
            {name: 'Carrie Smitch'},
            {name: 'Jesus Wohau'}
        ];
    }
}
```

```
<template>
  <label repeat.for="person of people">
    <input type="checkbox" model.bind="person" checked.bind="selectedPeople" />
    ${person.name}
  </label>
  
  <p>Selected people:</p>
  <ul if.bind="selectedPeople">
    <li repeat.for="person of selectedPeople">${person.name}</li>
  </ul>
</template>
```

**Checkbox with a Boolean**

```
export class MyViewModel {
    agreeToTerms = false;
}
```

```
<template>
  <label><input type="radio" name="terms" model.bind="true" checked.bind="agreeToTerms" />Yes</label>
  <label><input type="radio" name="terms" model.bind="false" checked.bind="agreeToTerms" />No</label>
  <br><br>
  <strong>${agreeToTerms ? 'I agree' : 'I disagree'}</strong>
</template>
```

## Binding To Radio Inputs
**Basic Radios**

```
export class MyViewModel {
    favoriteColor = null;
    colors = ['Red', 'Yellow', 'Pink', 'Green', 'Purple', 'Orange', 'Blue'];
}
```

```
<template>
  <label repeat.for="color of colors">
    <input type="radio" name="colors" value.bind="color" checked.bind="favoriteColor" />
    ${color}
  </label>
</template>
```

**Radios With Object Arrays**

```
export class MyViewModel {
    people = [];
    selectedPerson = null;
    
    constructor() {
        this.people = [
            {name: 'John Michaels'},
            {name: 'Gary Stevens'},
            {name: 'Carrie Smitch'},
            {name: 'Jesus Wohau'}
        ];
    }
}
```

```
<template>
  <label repeat.for="person of people">
    <input type="radio" name="person" model.bind="person" checked.bind="selectedPerson" />
    ${person.name}
  </label>
</template>
```

**Radios with a Boolean**

```
export class MyViewModel {
    agreeToTerms = null;
}
```

```
<template>
  <label><input type="radio" name="terms" model.bind="null" checked.bind="agreeToTerms" />No Answer</label>
  <label><input type="radio" name="terms" model.bind="true" checked.bind="agreeToTerms" />Yes</label>
  <label><input type="radio" name="terms" model.bind="false" checked.bind="agreeToTerms" />No</label>
</template>
```

## Binding To File Inputs
```
export class MyViewModel {
    selectedFiles;
}
```

```
<template>
    <input type="file" files.bind="selectedFiles">
</template>
```

## Binding To Select Elements
**Strings Array**

When selecting a value in the select dropdown and providing an array of strings, the selected value will be bound to the select element's value property as a string that we can display using string interpolation.

```
export class MyViewModel {
    animals = [];
    favouriteAnimal = null;

    constructor() {
        this.animals = [
            'Cat',
            'Dog',
            'Fish',
            'Rabbit',
            'Tiger',
            'Bat'
        ];
    }
}
```

```
<template>
    ${favouriteAnimal}
    <select name="animals" value.bind="favouriteAnimal">
        <option repeat.for="animal of animals">${animal}</option>
    </select>
</template>
```

**Objects Array**

Unlike the above example, when supplying an array of objects, when a value is selected in a dropdown, the model bound to that particular option is the supplied object.

```
export class MyViewModel {
    animals = [];
    favouriteAnimal = null;

    constructor() {
        this.animals = [
            {label: 'Cat', value: 99},
            {label: 'Dog', value: 493},
            {label: 'Fish', value: 934839200},
            {label: 'Rabbit', value: 8311},
            {label: 'Tiger', value: 22},
            {label: 'Bat', value: 3711}
        ];
    }
}
```

```
<template>
    <p>Favourite animal ID: ${favouriteAnimal.value}</p>
    <p>Favourite animal name: ${favouriteAnimal.label}</p>
    <select name="animals" value.bind="favouriteAnimal">
        <option repeat.for="animal of animals" model.bind="animal">${animal.label}</option>
    </select>
</template>
```

## Binding Styles
Binding to the browser native `style` attribute using Aurelia. If using string interpolation, you should use the `css` alias so styling works in Internet Explorer.

**Style String**

```
export class MyViewModel {
  constructor() {
    this.styleString = 'color: #F2D3D6; background-color: #333';
  }
}
```

```
<template>
  <div style.bind="styleString"></div>
</template>
```

**Style Object**

```
export class MyViewModel {
  constructor() {
    this.styles = {color: '#F2D3D6', 'background-color': '#333'};
  }
}
```

```
<template>
  <div style.bind="styles"></div>
</template>
```

**String Interpolation**

Very similar to string binding above, this allows you to use string interpolation to bind to styles instead. If any of the values change, they will be updated in the view accordingly.

***Note:** for Internet Explorer compatibility, we use the alias `css` to bind styles. This ensures that string interpolation works in Internet Explorer.*

```
export class MyViewModel {
    color = 'red';
    height = 350;
    width = 350;
}
```

```
<template>
    <div css="width: ${width}px; height: ${height}px; color:${color}">My Text</div>
</template>
```

## Conditionally Show & Hide a HTML Element
When using `show.bind` the element remains in the page and is either hidden or visible through the use of `display:none` or `display:block` behind the scenes.

```
export class MyViewModel {
    isVisible = false;
}
```

```
<template>
    <div show.bind="isVisible"><strong>I can be both hidden or visible, but not at the same time</strong></div>
</template>
```

## Conditionally Add or Remove a HTML Element
Unlike `show.bind` when using `if.bind` the element will be removed from the page if the supplied binding value is `false` or added into the page if the value is `true`.

```
export class MyViewModel {
    isVisible = false;
}
```

```
<template>
    <div if.bind="isVisible"><strong>If you can read this, I am still here.</strong></div>
</template>
```

**Grouped Elements**

Sometimes there might be a situation where you want to add or remove a whole group of elements at once. For this, we can use a `<template>` element to show or hide additional elements without the need for a placeholder element like a DIV.

```
export class MyViewModel {
    hasErrors = false;
    errorMessage = '';
}
```

```
<template if.bind="hasErrors">
    <i class="icon error"></i>
    ${errorMessage}
</template>
```

