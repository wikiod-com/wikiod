---
title: "Common Pitfalls & Best Practices"
slug: "common-pitfalls--best-practices"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Extend vs Override
### Overrides: 
Override file:

    Ext.define('MyApp.override.CornField',
        override: 'Ext.form.field.Text',
        initComponent: function () {
            this.callParent(arguments);
            this.setValue('Corn!');
        }
    );

Use in app:

    {
        xtype: 'textfield'
    }

### Extensions:

Override file:

    Ext.define('MyApp.form.field.CornField',
        extend: 'Ext.form.field.Text',
        alias: 'widget.cornfield',
        initComponent: function () {
            this.callParent(arguments);
            this.setValue('Corn!');
        }
    );

Use in app:

    {
        xtype: 'cornfield'
    }

### Explanation

ExtJS provides two main ways to change the behavior of existing classes: extending them, and overriding them. Each has benefits and pitfalls that should be considered before using them.

#### Extensions
Extending a class creates a new class that inherits its behavior and configuration from its parent. By creating a new class through extension, repeated configuration and behavioral changes can be made in a central location and reused throughout the application. The biggest advantage of extension is that the parent class remains intact and available for simpler use cases where the extended behavior is not desired.

Examples of good use cases for extensions include custom form fields with special behavior, specialized modals, and custom components in general.

#### Overrides
Overriding a class modifies the behavior of an existing class in place. Configuration and methods in overrides replace their parent class counterparts entirely, creating new default configurations and behavior that populate throughout the application. Overrides should be used sparingly because of the destructive nature of their use; an extended class can typically provide the same benefits while leaving the parent class undisturbed.

However, overrides can provide benefits in some situations. Good use cases include fixing bugs in existing classes, modifying proxy behavior to append extra information to requests, such as a token or session data, and generally forcing a specific behavior to be the default behavior across an application.

## Separate Overrides from Bug Fixes


## Separation of Concerns
### Worse

ViewController:

    // ...
    myMethod: function () {
        this.getView().lookup('myhappyfield').setValue(100);
    }
    //...

View:

    //...
    items: [
        {
            xtype: 'textfield',
            reference: 'myhappyfield'
        }
    ]
    //...

### Better

ViewController:

    // ...
    myMethod: function () {
        this.getView().setHappiness(100);
    }
    //...

View:

    //...
    items: [
        {
            xtype: 'textfield',
            reference: 'myhappyfield'
        }
    ],
    setHappiness: function (happiness) {
        this.lookup('myhappyfield').setValue(happiness);
    }
    //...

### Explanation
In this example, the two snippets of code perform the same task. However, in the event the reference to `myhappyfield` changes or the methodology of indicating 'happiness' changes significantly, the former approach requires changes each place the reference is used.

With separated concerns (the latter example), the view provides an abstracted way to modify 'happiness' that other classes may use. The querying and component manipulation are kept in one place (right alongside the view definition itself!) and the calls to the abstracted method need not change.

Although it is possible for a controller to allow querying down through the layers of a view, it is strongly advisable to abstract that behavior into methods on the view. In this way, a view can provide standardized ways for other classes to influence it and minimize or eliminate changes to other classes when the structure of a view changes.

