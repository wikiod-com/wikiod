---
title: "Custom Bindings"
slug: "custom-bindings"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Binding Registration
Cusom bindings should be registered by extending the current knockout bindingHandlers object.<br />This is done by adding a new property to the object.

<!--lang:js -->
    ko.bindingHandlers.newBinding = {
        init: function(element, valueAccessor, allBindings, viewModel, bindingContext) {
        },
        update: function(element, valueAccessor, allBindings, viewModel, bindingContext) {
        }
    };

## Custom fade in/fade out visibility binding
This example implements a custom binding that toggles visibility (similar to the existing [visible binding][1]), but will utilize jQuery's [fading][2] API to animate the transition from visible to invisible.

Custom binding definition:

    //Add a custom binding called "fadeVisible" by adding it as a property of ko.bindingHandlers
    ko.bindingHandlers.fadeVisible = {
      //On initialization, check to see if bound element should be hidden by default
      'init': function(element, valueAccessor, allBindings, viewModel, bindingContext){
        var show = ko.utils.unwrapObservable(valueAccessor());
        if(!show){
          element.style.display = 'none';
        }
      },
      //On update, see if fade in/fade out should be triggered. Factor in current visibility 
      'update': function(element, valueAccessor, allBindings, viewModel, bindingContext) {
        var show = ko.utils.unwrapObservable(valueAccessor());
        var isVisible = !(element.style.display == "none");
        
        if (show && !isVisible){
          $(element).fadeIn(750);
        }else if(!show && isVisible){
          $(element).fadeOut(750);
        }
      }
    }

Sample markup with the fadeVisible binding:

    <div data-bind="fadeVisible: showHidden()">
      Field 1: <input type="text" name="value1" />
      <br />
      Field 2: <input type="text" name="value2" />
    </div>
    <input data-bind="checked: showHidden" type="checkbox"/> Show hidden

Sample view model:

    var ViewModel = function(){
        var self = this;
        self.showHidden = ko.observable(false);
    }
    
    
    ko.applyBindings(new ViewModel());

  [1]: http://knockoutjs.com/documentation/visible-binding.html
  [2]: https://api.jquery.com/category/effects/fading/

## Custom text replace binding
This example is a custom binding that replaces text whenever an input value is updated. In this case, spaces will be replaced with "+". It is intended to be used alongside the existing [value binding][1], and shows binding with an object literal.

Sample markup with the replaceText binding:

    <input type="text" data-bind="value: myField, replaceText: {value: myField, find:' ', replace:'+'}" />

Custom binding definition:

    ko.bindingHandlers.replaceText = {

       //On update, grab the current value and replace text
      'update': function(element, valueAccessor, allBindings, viewModel, bindingContext) {

          //Get the current value of the input 
          var val = ko.utils.unwrapObservable(valueAccessor().value());

          //Replace text using passed in values obtained from valueAccessor()
          //Note - Consider using something like string.js to do the find and replace
          var replacedValue = val.split(valueAccessor().find).join(valueAccessor().replace);

          //Set new value
          valueAccessor().value(replacedValue);
      }
    }

    
Sample view model:

    var ViewModel = function(){
      var self = this;
      self.myField = ko.observable("this is a simple test");
    }
     
    
    ko.applyBindings(new ViewModel());





  [1]: http://knockoutjs.com/documentation/value-binding.html

## Replace with regular expression custom binding
**Custom binding definition**

    function regExReplace(element, valueAccessor, allBindingsAccessor, viewModel, bindingContext) {
    
      var observable = valueAccessor();
      var textToReplace = allBindingsAccessor().textToReplace || '';
      var pattern = allBindingsAccessor().pattern || '';
      var flags = allBindingsAccessor().flags;
      var text = ko.utils.unwrapObservable(valueAccessor());
      if (!text) return;
      var textReplaced = text.replace(new RegExp(pattern, flags), textToReplace);
    
      observable(textReplaced);
    }
    
    ko.bindingHandlers.regExReplace = {
      init: regExReplace,
      update: regExReplace
    }

**Usage**

*ViewModel*

    ko.applyBindings({
      name: ko.observable(),
      num: ko.observable()
    });
*View*

    <input type="text" data-bind="textInput : name, regExReplace:name, pattern:'(^[^a-zA-Z]*)|(\\W)',flags:'g'" placeholder="Enter a valid name" />
    <span data-bind="text : name"></span>
    <br/>
    <input class=" form-control " type="text " data-bind="textInput : num, regExReplace:num, pattern: '[^0-9]',flags: 'g' " placeholder="Enter a number " />
    <span data-bind="text : num"></span>



