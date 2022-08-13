---
title: "Equivalents of AngularJS bindings"
slug: "equivalents-of-angularjs-bindings"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Not everything in AngularJS has a KnockoutJS equivalent (for example `ngCloack`, or `ngSrc`). There are two main solutions typically available:

1. Use the generic `attr` or `event` binding instead.
2. Similar to custom directives in AngularJS, you can write your own [custom binding handler][1] if you need something that isn't included in the base library.

If you prefer the AngularJS binding syntax you can consider using [Knockout.Punches][2] which enables handlebar-style binding.

  [1]: http://knockoutjs.com/documentation/custom-bindings.html
  [2]: https://mbest.github.io/knockout.punches/

## ngShow
AngularJS code for dynamically showing/hiding an element:

<!-- language-all: lang-html -->

    <p ng-show="SomeScopeProperty">This is conditionally shown.</p>

KnockoutJS equivalent:

    <p data-bind="visible: SomeScopeObservable">This is conditionally shown.</p>

## ngBind (curly markup)
AngularJS code for rendering plain text:

<!-- language-all: lang-html -->

    <p>{{ ScopePropertyX }} and {{ ScopePropertyY }}</p>

KnockoutJS equivalent:

    <p>
      <!-- ko text: ScopeObservableX --><!-- /ko --> 
      and 
      <!-- ko text: ScopeObservableY --><!-- /ko --> 
    </p>

or:

    <p>
      <span data-bind="text: ScopeObservableX"></span> 
      and 
      <span data-bind="text: ScopeObservableY"></span>
    </p>

## ngModel on input[type=text]
AngularJS code for two-way binding on a text `input`:

<!-- language-all: lang-html -->

    <input ng-model="ScopePropertyX" type="text" />

KnockoutJS equivalent:

    <input data-bind="textInput: ScopeObservableX" type="text" />

## ngHide
There is no *direct* equivalent binding in KnockoutJS. However, since hiding is just the opposite of showing, we can just invert [the example for Knockout's `ngShow` equivalent][1].

<!-- language: lang-html -->

    <p ng-hide="SomeScopeProperty">This is conditionally shown.</p>

KnockoutJS equivalent:

<!-- language: lang-html -->

    <p data-bind="visible: !SomeScopeObservable()">This is conditionally hidden.</p>

The above KnockoutJS example assumes `SomeScopeObservable` is an observable, and because we use it in an expression (because of the `!` operator in front of it) we cannot omit the `()` at the end.

  [1]: https://www.wikiod.com/knockout-js/equivalents-of-angularjs-bindings#ngShow

## ngClass
AngularJS code for dynamic classes:

<!-- language: lang-html -->

    <p ng-class="{ highlighted: scopeVariableX, 'has-error': scopeVariableY }">Text.</p>

KnockoutJS equivalent:

<!-- language: lang-html -->

    <p data-bind="css: { highlighted: scopeObservableX, 'has-error': scopeObservableY }">Text.</p>

