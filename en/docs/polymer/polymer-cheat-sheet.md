---
title: "Polymer Cheat Sheet"
slug: "polymer-cheat-sheet"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This is a cheat sheet for the Polymer 2.x library. Fork of [post](https://meowni.ca/posts/polymer-2-cheatsheet/) from Monica Dinculescu.

## Defining an element
<!-- language-all: lang-html -->

Docs: [1.x -> 2.x upgrade guide](https://www.polymer-project.org/2.0/docs/upgrade),
[registering an element](https://www.polymer-project.org/2.0/docs/devguide/registering-elements),
[shared style modules](https://www.polymer-project.org/2.0/docs/devguide/style-shadow-dom#style-modules).

    <link rel="import" href="bower_components/polymer/polymer-element.html">
    <dom-module id="element-name">
      <template>
        <!-- Use one of these style declarations, but not both -->
        <!-- Use this if you don’t want to include a shared style -->
        <style></style>
        <!-- Use this if you want to include a shared style -->
        <style include="some-style-module-name"></style>
      </template>
      <script>
        class MyElement extends Polymer.Element {
          static get is() { return 'element-name'; }
          // All of these are optional. Only keep the ones you need.
          static get properties() { ... }
          static get observers() { ... }
        }
    
        // Associate the new class with an element name
        customElements.define(MyElement.is, MyElement);
      </script>
    </dom-module>

To get the class definition for a particular custom tag, you can use
`customElements.get('element-name')`.

## Extending an element
<!-- language-all: lang-html -->

Docs: [extending elements](https://www.polymer-project.org/2.0/docs/devguide/custom-elements#extending-other-elements), [inherited templates](https://www.polymer-project.org/2.0/docs/devguide/dom-template#inherited-templates).

Instead of `Polymer.Element`, a custom element can extend a different element:

<!-- language: lang-js -->
    class ParentElement extends Polymer.Element {
      /* ... */
    }
    class ChildElement extends ParentElement {
      /* ... */
    }

To change or add to the parent's template, override the `template` getter:

    <dom-module id="child-element">
      <template>
        <style> /* ... */ </style>
        <span>bonus!</span>
       </template>
      <script>
        var childTemplate;
        var childTemplate = Polymer.DomModule.import('child-element', 'template');
        var parentTemplate = ParentElement.template.cloneNode(true);
        // Or however you want to assemble these.
        childTemplate.content.insertBefore(parentTemplate.firstChild, parentTemplate);
    
        class ChildElement extends ParentElement {
          static get is() { return 'child-element'; }
          // Note: the more work you do here, the slower your element is to
          // boot up. You should probably do the template assembling once, in a
          // static method outside your class (like above).
          static get template() {
            return childTemplate;
          }
        }
        customElements.define(ChildElement.is, ChildElement);
      </script>
    </dom-module>

If you don't know the parent class, you can also use:

<!-- language: lang-js -->
    class ChildElement extends customElements.get('parent-element') {
      /* ... */
    }


## Defining a mixin
<!-- language-all: lang-html -->

Docs: [mixins](https://www.polymer-project.org/2.0/docs/devguide/custom-elements#mixins),
[hybrid elements](https://www.polymer-project.org/2.0/docs/devguide/hybrid-elements).

Defining a class expression mixin to share implementation between different elements:

    <script>
      MyMixin = function(superClass) {
        return class extends superClass {
          // Code that you want common to elements.
          // If you're going to override a lifecycle method, remember that a) you
          // might need to call super but b) it might not exist.
          connectedCallback() {
            if (super.connectedCallback) {
              super.connectedCallback();
            }
            /* ... */
          }
        }
      }
    </script>

Using the mixin in an element definition:

    <dom-module id="element-name">
      <template><!-- ... --></template>
      <script>
        // This could also be a sequence:
        // class MyElement extends AnotherMixin(MyMixin(Polymer.Element)) { … }
        class MyElement extends MyMixin(Polymer.Element) {
          static get is() { return 'element-name' }
          /* ... */
        }
        customElements.define(MyElement.is, MyElement);
      </script>
    </dom-module>

Using hybrid behaviors (defined in the 1.x syntax) as mixins:

    <dom-module id="element-name">
      <template><!-- ... --></template>
      <script>
        class MyElement extends Polymer.mixinBehaviors([MyBehavior, MyBehavior2], Polymer.Element) {
         static get is() { return 'element-name' }
         /* ... */
        }
        customElements.define('element-name', MyElement);
      </script>
    </dom-module>


## Lifecycle methods
<!-- language-all: lang-js -->

Docs: [lifecycle callbacks](https://www.polymer-project.org/2.0/docs/devguide/custom-elements#element-lifecycle),
[ready](https://www.polymer-project.org/2.0/docs/devguide/custom-elements#one-time-initialization).

    class MyElement extends Polymer.Element {
     constructor() { super(); /* ... */ }
     ready() { super.ready(); /* ... */ }
     connectedCallback() { super.connectedCallback(); /* ... */ }
     disconnectedCallback() { super.disconnectedCallback(); /* ... */ }
     attributeChangedCallback() { super.attributeChangedCallback(); /* ... */ }
    }



## Data binding
<!-- language-all: lang-html -->

Docs: [data binding](https://www.polymer-project.org/2.0/docs/devguide/data-binding),
[attribute binding](https://www.polymer-project.org/2.0/docs/devguide/data-binding#attribute-binding),
[binding to array items](https://www.polymer-project.org/2.0/docs/devguide/data-binding#bind-array-item),
[computed bindings](https://www.polymer-project.org/2.0/docs/devguide/data-binding#annotated-computed).

Don't forget: Polymer [camel-cases](https://www.polymer-project.org/2.0/docs/devguide/properties#property-name-mapping) properties, so if in JavaScript you use `myProperty`,
in HTML you would use `my-property`.

**One way** binding: when `myProperty` changes, `theirProperty` gets updated:

    <some-element their-property="[[myProperty]]"></some-element>

**Two way** binding: when `myProperty` changes, `theirProperty` gets updated,
and vice versa:

    <some-element their-property="{{myProperty}}"></some-element>

**Attribute binding**: when `myProperty` is `true`, the element is hidden; when it's
`false`, the element is visible. The difference between attribute and property
binding is that property binding is equivalent to `someElement.someProp = value`,
whereas attribute binding is equivalent to: `someElement.setAttribute(someProp, value)`

    <some-element hidden$="[[myProperty]]"></some-element>

**Computed binding**: binding to the `class` attribute will recompile styles when
`myProperty` changes:

    <some-element class$="[[_computeSomething(myProperty)]]"></some-element>
    <script>
    _computeSomething: function(prop) {
      return prop ? 'a-class-name' : 'another-class-name';
    }
    </script>


## Observers
<!-- language-all: lang-js -->

Docs: [observers](https://www.polymer-project.org/2.0/docs/devguide/observers),
[multi-property observers](https://www.polymer-project.org/2.0/docs/devguide/observers#multi-property-observers),
[observing array mutations](https://www.polymer-project.org/2.0/docs/devguide/observers#array-observation),
[adding observers dynamically](https://www.polymer-project.org/2.0/docs/devguide/observers#dynamic-observers).

Adding an `observer` in the `properties` block lets you observe changes in the
value of a property:

    static get properties() {
      return {
        myProperty: {
          observer: '_myPropertyChanged'
        }
      }
    }
    
    // The second argument is optional, and gives you the
    // previous value of the property, before the update:
    _myPropertyChanged(value, /*oldValue */) { /* ... */ }

In the `observers` block:

    static get observers() {
      return [
        '_doSomething(myProperty)',
        '_multiPropertyObserver(myProperty, anotherProperty)',
        '_observerForASubProperty(user.name)',
        // Below, items can be an array or an object:
        '_observerForABunchOfSubPaths(items.*)'
      ]
    }

Adding an observer dynamically for a property `otherProperty`:

    // Define a method.
    _otherPropertyChanged(value) { /* ... */ }
    // Call it when `otherPropety` changes.
    this._createPropertyObserver('otherProperty', '_otherPropertyChanged', true);



