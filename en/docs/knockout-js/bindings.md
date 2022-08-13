---
title: "Bindings"
slug: "bindings"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Syntax
 - `<!-- ko if:myObservable --><!-- /ko --> `
 - `<i data-bind="if:myObservable"></i>`

# What a binding is

Essentially a binding or a data binding is a way to link your ViewModels to your Views(templates) and vice versa. KnockoutJS uses two-way data binding, which means changes to your ViewModel influence the View and changes to your View can influence the ViewModel.

## Under the hood (short overview)

Bindings are just plugins (scripts) that allow you to solve a particular task. This task is more often than not is changing markup (html) according to your ViewModel.

For example, a `text` binding allows you to display text and dynamically change it whenever your ViewModel changes.

KnockoutJS comes with many powerful bindings and lets you extend it with your own custom bindings.

And most importantly bindings are not magical, they work according to a set of rules and anytime you are unsure of what a binding does, what parameters it takes or when it will update the view you can refer to source code of the binding.

Consider the following example of a custom binding:

    ko.bindingHandlers.debug = {
        init: function (element, valueAccessor, allBindingsAccessor, viewModel, bindingContext) {
            ko.computed(function () {
                var value = ko.unwrap(valueAccessor());
        
                console.log({
                    value: value,
                    viewModel: viewModel,
                    bindingContext: bindingContext
                });
            }, null, { disposeWhenNodeIsRemoved: element });
        }
    };
    
 0. A binding has a name - `debug` so you can use as follows:

`data-bind="debug: 'foo'"`

 1. The `init` method is called once when the binding is initiated. The rest of the updates are handled by a anonymous computed which is disposed when `element` is removed.
 2. The binding prints to console several things: the passed value in our example this value is `foo` (this value can also be observable since `ko.unwrap` method is used to read it), the current viewModel and bindingContext.
 4. Whenever the passed value changes the binding will print updated information to console.
 5. This binding cannot be used with virtual elements (in html comments), only on real elements, since `ko.virtualElements.allowedBindings.debug` flag is not set to true.

 


# When to use parentheses

Without any additional [plugins][1], KnockoutJS will only have live View updates for properties on the ViewModel that are *observable* (regular `observable`, but also `computed`, `pureComputed`, `observableArray`, etc). An observable would be created like this:

    var vm = { name: ko.observable("John") };

In this case, `vm.name` is a *function* with two seperate "modes":

1. Getter: `vm.name()`, without arguments, will get the current value;
2. Setter: `vm.name("Johnnyboy")`, with an argument, will set a new value.

In the built-in data-bindings you can *always* use the getter form, and you can *sometimes* actually *omit* the parentheses, and the binding will effectively "add" them for you. So these are equivalent:

    <p data-bind="text: name"></p> ... will work
    <p data-bind="text: name()"></p> ... works too

But this will fail:

    <p data-bind="text: 'Hello, ' + name + '!'"></p> ... FAILS!
    
Because as soon as you want to "do" something before passing a value to a data-binding, including value comparisons, you need to properly "get" the values for all observables, e.g.:

    <p data-bind="text: 'Hello, ' + name() + '!'"></p> ... works

See also [this Q&A][2] for more details.


  [1]: https://github.com/SteveSanderson/knockout-es5
  [2]: http://stackoverflow.com/q/10996185/419956

## Foreach
Similar to repeaters used in other languages. This binding will allow you to replicate a block of html for each item in an array. 

    <div data-bind="foreach:contacts">
        <div class="contact">
            <h2 data-bind="text:name">
            <p data-bind="text:info">
        </div>
    </div>
     
    <script type="text/javascript">
        var contactViewModel = function (data) {
            this.name = ko.observable(data.name);
            this.info = ko.observable(data.info);
        };
    
        ko.applyBindings({
            contacts: [
                new contactViewModel({name:'Erik', info:'Erik@gmail.com'}),
                new contactViewModel({name:'Audrey', info:'Audrey@gmail.com'})
            ]
        });
    </script>

Notice that when we are looping through our context becomes the item within the array, in this case an instance of the `contactViewModel`.  Within a `foreach` we also have access to 
 - `$parent` - the view model that created this binding 
 - `$root` - the root view model (could also be parent) 
 - `$data` - the data at this index of the array
 - `$index` - the (observable) zero-based index of the rendered item

## With
The `with` binding binds the HTML inside the bound node to a separate context:

<!-- lang: html -->
    <div data-bind="with: subViewModel">
      <p data-bind="text: title"></p>
    </div>

The `with` binding may also be used without a container element where a container element may not be appropriate.
<!-- lang: html -->
    <!-- ko with: subViewModel -->
      <p data-bind="text: title"></p>
    <!-- /ko -->

<!-- lang: js -->
    
    var vm = {
      subViewModel: ko.observable()
    };

    // Doesn't throw an error on the `text: title`; the `<p>` element 
    // isn't bound to any context (and even removed from the DOM)
    ko.applyBindings(vm);

    // Includes the `<p>` element and binds it to our new object
    vm.subViewModel({ title: "SubViewModel" });

The `with` binding has many similarities to the `template` or `foreach` bindings.
       

## If / ifnot
You can use the `if` binding to determine whether or not the child elements of the node should be created. 

    <div class="product-info">
        <h2>  Product1  </h2>
        <img src="../products/product1.jpg"/>
        <span data-bind="if:featured">
            <span class="featured"></span>
        </span>
        <span data-bind="ifnot:inStock">
            <span class="out-of-stock"></span>
        </span>
    </div>
         
    <script>
        ko.applyBindings({
            product: {
                featured: ko.observable(true),
                inStock: ko.observable(false)
            }
        });
    </script>

The inverse of the `if` binding is `ifnot`
            
    <div data-bind="ifnot: someProperty">...</div>

is equivalent to 

    <div data-bind="if: !someProperty()">...</div>

Sometimes, you'll wan't to control the presence of elements without having to create a container (typically for `<li>` elements in a `<ul>` or `<option>` elements inside a `<select>`)

Knockout enables this with containerless control flow syntax based on comment tags like so:

    <select>
      <option value="0">fixed option</option>
    <!-- ko if: featured-->
      <option value="1">featured option</option>
    <!-- /ko -->
    </select>

## Visible
The `visible` binding will hide an element by applying `style="display: none;"` to it when the binding evaluate as falsey.

    <input type="text" data-bind="textInput: name"> <span class="error" data-bind="visible: isInvalid">Required!</span>

    ko.applyBindings(new ViewModel());
    
    function ViewModel(){
        var vm = this;
      vm.name = ko.observable("test");
      vm.isInvalid = ko.computed(function() {
          return vm.name().length == 0;
      });
    }
[jsFiddle][1]


  [1]: https://jsfiddle.net/gv3qg2p7/

