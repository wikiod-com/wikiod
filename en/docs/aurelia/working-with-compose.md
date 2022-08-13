---
title: "Working with <compose>"
slug: "working-with-compose"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Compose with View only
Presumably the simplest way to use `compose` is with a View only. This allows you to include HTML templates without the need to declare a ViewModel with bindable properties for each of them, making it easier to reuse smaller pieces of HTML.

The BindingContext (ViewModel) of the View will be set to that of the parent ViewModel.

**Usage**:

src/app.html

    <template>
      <compose view="./greeter.html"></compose>
    </template>

src/greeter.html

    <template>
        <h1>Hello, ${name}!</h1>
    </template>

src/app.ts

    export class App {
      /* This property is directly available to the child view
         because it does not have its own ViewModel */
      name = "Rob"; 
    }



## Compose with View, ViewModel and Model
Using `compose` with a View, ViewModel and Model is an easy way to reuse and combine different Views and ViewModels.

**Given the following View and ViewModel (applies to each alternative below):**

src/greeter.html

    <template>
        <h1>Hello, ${name}!</h1>
    </template>

src/greeter.ts

    export class Greeter {
        name: string;
    
        /* The object that is bound to the model property of the compose element,
           will be passed into the ViewModel's activate method
        */
        activate(model) {
            this.name = model.name;
        }
    }

**1 - Basic usage, inline model**:

src/app.html

    <template>
      <compose view="./greeter.html" view-model="./greeter" model="{name: 'Rob'}"></compose>
    </template>

src/app.ts

    export class App {
    }

**2 - Basic usage, databound model**:

src/app.html

    <template>
      <compose view="./greeter.html" view-model="./greeter" model.bind="model"></compose>
    </template>

src/app.ts

    export class App {
      model = {
        name: Rob"
      };
    }




**3 - Updating the DOM when model properties change**:

The only drawback of the second approach is that the `model`'s properties are not observed, so any changes them will not propagate to the DOM. 

One way to overcome this is by making sure the `model` property itself changes whenever any of its properties change. This will cause the `compose` element to be re-compiled:

src/app.html

    <template>
      <compose view="./greeter.html" view-model="./greeter" model.bind="model"></compose>
      <input type="text" value.two-way="name">
    </template>

src/app.ts

    import { computedFrom } from "aurelia-framework";
    
    export class App {
      name: string;
      
      @computedFrom("name")
      get model() {
        return {
          name: this.name
        };
      }

      /* Using computedFrom prevents "dirty checking" and is optional,
         but highly recommended for performance reasons.

         Simply pass an array with names of all properties you wish to "observe".
         Expressions / nested properties are not supported.
      */
    }

