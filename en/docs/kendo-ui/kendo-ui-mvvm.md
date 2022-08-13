---
title: "Kendo UI MVVM"
slug: "kendo-ui-mvvm"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Kendo MVVM is one of the JavaScript MVVM framework. It is the implementation of the MVVM pattern.

It create a definition for the data that we want to display and manipulate (the Model), the HTML markup that create structure for overall web page (the View), and the JavaScript code that handles user input, reacts to events, and transforms the static markup into dynamic elements (the View-Model).

Kendo MVVM is the JavaScript MVVM framework that implements the MVVM pattern.

## Basic binding
**View :**

    <form id="form">
        <label>First Name: <input data-bind="value: firstName" /></label>
        <label>Last Name: <input data-bind="value: lastName" /></label>
        <label>Gender:
            <select data-bind="source: genders, value: gender"></select>
        </label>
        <label><input type="checkbox" data-bind="checked: agreed" /> I have read the licence agreement</label>
        <button data-bind="enabled: agreed, click: register">Register</button>
        <div data-bind="visible: confirmed">
            Thank you for your registration, <span data-bind="text: firstName"></span> <span data-bind="text: lastName"></span>
        </div>
    </form>

**View-model :**

    var viewModel = kendo.observable({
            firstName: "Arif",
            lastName: "Rahman",
            genders: ["Male", "Female"],
            gender: "Male",
            agreed: false,
            confirmed: false,
            register: function(e) {
                e.preventDefault();
    
                this.set("confirmed", true);
            }
        });
    
        kendo.bind($("form"), viewModel);

For demo [VISIT JSFIDDLER EXAMPLE](https://jsfiddle.net/Arif2009/fme6m8so/)

A binding pairs a DOM element (or widget) property to a field or method of the View-Model. Bindings are specified via the **data-bind** attribute in the form **binding name: view model field or method**, e.g. **value: firstName**. Some bindings were used in the aforementioned example: **value**, **source**, **visible**, **enabled** and **click**.

The Kendo UI MVVM supports binding to other properties as well: **html**, **attr** etc. The **data-bind** may contain a comma-separated list of bindings e.g. **data-bind="enabled: agreed, click: register"**.

