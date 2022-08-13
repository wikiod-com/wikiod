---
title: "Custom widgets for fields"
slug: "custom-widgets-for-fields"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

- make sure you properly [add javascript file to your module][1]
- do not forget to add 'web' as dependency in \_\_openerp\_\_.py:
```Python
'depends': ['web',....]
```

  [1]: https://www.wikiod.com/odoo-8/add-css-and-javascript-files-to-odoo-module

## Custom widget for numeric fields to use in TreeView
The below example widget demonstrates how to format individual cells of a TreeView column conditionally, depending on value of the field in the particular cell. If value of field is negative, then it'll be displayed in red color and minus symbol will be hidden, otherwise it'll be displayed in normal color .

A widget should be written in JavaScript, lets use `custom_widget_name` as a name for a new widget, and `your_module_name` is a technical name of your module (same as your module's root directory name)

Uunder static/src/js/ folder in your module add javascript file (say static/src/js/*custom_widget*.js) with a custom widget in it:

     openerp.your_module_name = function (instance) {

        instance.web.list.columns.add('field.custom_widget_name', 'instance.your_module_name.custom_widget_name');

        instance.your_module_name.custom_widget_name = instance.web.list.Column.extend({
            _format: function (row_data, options) {
                res = this._super.apply(this, arguments);
                var amount = parseFloat(res);
                if (amount < 0){
                    return "<font color='#ff0000'>"+(-amount)+"</font>";
                }
                return res
            }
        });
        //
        //here you can add more widgets if you need, as above...
        //
    };

the above example widget can be used in a list view for field of type float and it applies custom rules as follows:
 - Negative numbers:
   -  Are shown in red.
   -  Minus symbol (a '-' character) is "hidden".
 - For positive numbers default layout is used.  


&nbsp;


This example widget can be applied to a field in a tree view of Odoo. You can use widget like this for a column you need to apply the custom rules to:
```XML
. . .
<tree >
    . . .
    <field name="some_field" widget="my_widget" />
    . . .
</tree>
. . .
```


