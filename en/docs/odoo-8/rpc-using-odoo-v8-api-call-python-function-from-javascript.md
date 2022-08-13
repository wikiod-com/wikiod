---
title: "RPC using Odoo v8 API (Call Python function from JavaScript)"
slug: "rpc-using-odoo-v8-api-call-python-function-from-javascript"
draft: false
images: []
weight: 9873
type: docs
toc: true
---

If you are considering to add new methods in Python to use them in RPC from JavaScript, then consider the following options of method decorators: if you've to deal with ids/recordsets then for python method definition choose decorator:

 - @api.multi - to get *recordset* in your method
 - @api.one - to get *browse_records* one by one in your method
in above examples @api.multi is used, but @api.one also may be used to deal with ids, depending on requirements (However, it's strongly recommended to use @api.multi instead of @api.one for performance reasons).

Or if it's simple function that does not have to deal with records/ids then for python method choose decorator:

 - @api.model - Allows to be polite with old style API.
 - @api.multi - Again, you can use it here as well, just pass `[ ]` (empty array) as first argument in javascript...

&nbsp;

References: [Odoo RPC documentation][1], [Odoo 8 API method decorators][2]


  [1]: https://www.odoo.com/documentation/8.0/reference/javascript.html#rpc
  [2]: https://www.odoo.com/documentation/8.0/reference/orm.html#module-openerp.api

## An example Odoo model to call methods from

    class my_model(models.Model):
        _name = "my.model"
    
        name = fields.Char('Name')
    
        @api.multi
        def foo_manipulate_records_1(self):
            """ function returns list of tuples (id,name) """
            return [(i.id,i.name) for i in self]
    
        @api.multi
        def foo_manipulate_records_2(self, arg1, arg2)
            #here you can take advantage of "self" recordset and same time use aditional arguments "arg1", "arg2"
            pass
    
        @api.model
        def bar_no_deal_with_ids(self, arg1, arg2):
            """ concatenate arg1 and arg2 """
            return unicode(arg1) + unicode(arg2)




## Odoo RPC examples
Examples below demonstrate how to call Python function from JavaScript in Odoo 8. In the examples we call methods of *my_model* described early on this page.  
We assume that in the following examples "list_of_ids" variable contains list(array) of ids of existing records of "my.model" model. 

- Call of method **foo_manipulate_records_1** decorated with **@api.multi**:
```JavaScript

    new instance.web.Model("my.model")
        .call( "foo_manipulate_records_1", [list_of_ids])
            .then(function (result) {
                // do something with result
        });

```  
- Call of method **foo_manipulate_records_2** decorated with **@api.multi**:
```JavaScript

    new instance.web.Model("my.model")
        .call( "foo_manipulate_records_2", [list_of_ids, arg1, arg2])
            .then(function (result) {
                // do something with result
        });

```
- Call of method **bar_no_deal_with_ids** decorated with **@api.model**:
```JavaScript
    new instance.web.Model("my.model")
        .call( "bar_no_deal_with_ids", [arg1, arg2])
            .then(function (result) {
                // do something with result
        });
```  
  
&nbsp;
  
Also if it has some sense depending on implementation, then you can call function decorated with @api.multi even if you have not to deal with ids (just pass empty array in place of ids, as first element of argument list):
```JavaScript
    new instance.web.Model("my.model")
        .call( "foo_manipulate_records_2", [[], arg1, arg2])
            .then(function (result) {
                // do something with result
        });
```
this way may be useful in some cases, as undecorated function in v8.0 api is considered as @api.multi (as @api.multi is a default decorator)
  
&nbsp;
  
Cxcept of two parameters to RPC call that are used in the above examples (the function name and argument list), you can use **third parameter** - a **dictionary of keyword arguments**. It's highly recommended to turn around a context (in some cases it might be even necessary), as it may change behavior of remote procedure (localization, etc.). See below the example with context argument in RPC call (same may be applied to all examples above)

    var self = this;
    new instance.web.Model("my.model")
        .call("foo_manipulate_records_2", [[], arg1, arg2], {'context':self.session.user_context})
            .then(function (result) {
                // do something with result
        });

Of course you can use custom context as well, if necessary, instead of turning around the existing one as in this example.




