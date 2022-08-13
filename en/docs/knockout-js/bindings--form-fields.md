---
title: "Bindings - Form fields"
slug: "bindings---form-fields"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Click
The `click` binding can be used with any visible DOM element to add an event handler, that will invoke a JavaScript function, when element is clicked.

<!-- language: html -->
    <button data-bind="click: onClick">Click me</button>


<!-- language: js -->
    ko.applyBindings({
      onClick: function(data, event) {
        // data: the context of the element that triggered the event
        console.log(data);

        // event: the click event
        console.log(event);
      }
    });
    

## Options
Use this binding to build options for a select item

    <select data-bind="options: gasGiants"></select>
     
    <script type="text/javascript">
        var viewModel = {
            gasGiants: ko.observableArray(['Jupiter', 'Saturn', 'Neptune', 'Uranus'])
        };
    </script>
You can also use properties inside the array for displaying in the list and for saving in the viewModel:
`optionsText` enables a custom display text

`optionsValue` sets the value property of the corresponding `<option>`

`value` stores the value of the selected option into an observable of the viewModel

    <select data-bind="options: gasGiants, optionsText:'name', optionsValue:'id', value:selectedPlanetId"></select>
     
    <script type="text/javascript">
        var viewModel = {
            selectedPlanetId: ko.observable(),
            gasGiants: ko.observableArray([{name:'Jupiter', id:'0'},
                                           {name:'Saturn', id:'1'},
                                           {name:'Neptune', id:'2'},
                                           {name:'Uranus', id:'3'}])
        };
    </script>

To store the results of a multi-select list, the options binding can be combined with the `selectedOptions` binding. 
   

    <select data-bind="options: gasGiants, selectedOptions: chosenGasGiants" multiple="true"></select>

    <script type="text/javascript">
        var viewModel = {
            gasGiants: ko.observableArray(['Jupiter', 'Saturn', 'Neptune', 'Uranus'])
            chosenGasGiants: ko.observableArray(['Jupiter','Saturn']) // Initial selection
        }; </script>




## disabled / enabled
The disabled binding adds a `disabled` attribute to a html element causing it to no longer be editable or clickable.
This is useful mainly for `<input>`, `<select>`, `<textarea>`, `<a>` and `<button>` elements

    <input data-bind="disabled: disableInput"/> 
    
    <script type="text/javascript">
    var viewModel = {
        disableInput: ko.observable(true);
    };
    </script>
The inverse of the `disabled` binding is `enabled`

The visibility can also be calculated using JavaScript functions. Any observables used in this functions have to called with parentheses
    <input data-bind="disabled: !disableInput()"/> 
    
    <script type="text/javascript">
    var viewModel = {
        disableInput: ko.observable(true);
    };
    </script>


or

    <input data-bind="disabled: allValues().length>4"/> 
    
    <script type="text/javascript">
    var viewModel = {
        allValues: ko.observableArray([1,2,3,4,5]);
    };
    </script>



## submit
Event handler to be invoked when a DOM element is submitted.

    <form data-bind="submit: doSomething">
        <!-- form content here -->
        <button type="submit"></button>
    </form>

    <script type="text/javascript">    
        var vm = { 
            doSomething: function(data){
                //do something here
            }; 
        }
    </script>

Knockout will prevent the browser's default submit action for that form. If you want your form to be submitted like a normal HTML form, you just return `true` in the submit handler.

## Value
Use the [value binding][1] to obtain the value of an element. The value binding can be applied to any form control, however there are other bindings that may be better suited for checkboxes, radio buttons, and text inputs.

The following example illustrates how to apply the binding element to several form input fields, and how to populate default values:

ViewModel definition:

    var MyViewModel = function(){
        var self = this;
      //Initialize valueOne
      self.valueOne = ko.observable();
      //Initialize valueTwo with a default value of "Value two"
      self.valueTwo = ko.observable("Value two");
      //Initialize the color dropdown, and by default, select the "blue" option
      self.color = ko.observable("blue");
      
      self.valueOne.subscribe(function(newValue){
          console.log("valueOne: " + newValue);
      });
      
      self.valueTwo.subscribe(function(newValue){
          console.log("valueTwo: " + newValue);
      });
      
      self.color.subscribe(function(newValue){
          console.log("color: " + newValue);
      });
    }

Associated markup:

    <input type="text" data-bind="value: valueOne" />
    <input type="text" data-bind="value: valueTwo" />
    
    <select data-bind="value: color">
      <option value="red">Red</option>
      <option value="green">Green</option>
      <option value="blue">Blue</option>
    </select>

In the above example, when a value changes, the new value will be logged to the console. The initial values will not trigger a change event.

By default, the value binding defines a change as a change to the elements value, and focus being transferred to another element. This can be altered using the valueUpdate option:

    <input type="text" data-bind="value: valueOne, valueUpdate: 'keyup'" />

The above example will change the value update to trigger on key up. Available options are input, keyup, keypress, and afterkeydown.


  [1]: http://knockoutjs.com/documentation/value-binding.html

