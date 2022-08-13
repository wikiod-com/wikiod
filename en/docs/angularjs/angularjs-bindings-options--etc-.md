---
title: "AngularJS bindings options (`=`, `@`, `&` etc.)"
slug: "angularjs-bindings-options-=---etc"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

Use [this plunker][1] to play with examples.


  [1]: http://plnkr.co/edit/mvOzMPaElILDmYDGaLiZ?p=preview

## @ one-way binding, attribute binding.
Pass in a literal value (not an object), such as a string or number.

Child scope gets his own value, if it updates the value, parent scope has his own old value (child scope can't modify the parens scope value). When parent scope value is changed, child scope value will be changed as well.
All interpolations appears every time on digest call, not only on directive creation.

    <one-way text="Simple text." <!-- 'Simple text.' -->
             simple-value="123" <!-- '123' Note, is actually a string object. -->
             interpolated-value="{{parentScopeValue}}" <!-- Some value from parent scope. You can't change parent scope value, only child scope value. Note, is actually a string object. -->
             interpolated-function-value="{{parentScopeFunction()}}" <!-- Executes parent scope function and takes a value. -->
             
             <!-- Unexpected usage. -->
             object-item="{{objectItem}}" <!-- Converts object|date to string. Result might be: '{"a":5,"b":"text"}'. -->
             function-item="{{parentScopeFunction}}"> <!-- Will be an empty string. -->
    </one-way>

## = two-way binding.
Passing in a value by reference, you want to share the value between both scopes and manipulate them from both scopes.
You should not use {{...}} for interpolation.

    <two-way text="'Simple text.'" <!-- 'Simple text.' -->
             simple-value="123" <!-- 123 Note, is actually a number now. -->
             interpolated-value="parentScopeValue" <!-- Some value from parent scope. You may change it in one scope and have updated value in another. -->
             object-item="objectItem" <!-- Some object from parent scope. You may change object properties in one scope and have updated properties in another. -->
             
             <!-- Unexpected usage. -->
             interpolated-function-value="parentScopeFunction()" <!-- Will raise an error. -->
             function-item="incrementInterpolated"> <!-- Pass the function by reference and you may use it in child scope. -->
    </two-way>

Passing function by reference is a bad idea: to allow scope to change the definition of a function, and two unnecessary watcher will be created, you need to minimize watchers count.

## & function binding, expression binding.
Pass a method into a directive. It provides a way to execute an expression in the context of the parent scope.
Method will be executed in the scope of the parent, you may pass some parameters from the child scope there.
You should not use {{...}} for interpolation.
When you use & in a directive, it generates a function that returns the value of the expression evaluated against the parent scope (not the same as = where you just pass a reference).

    <expression-binding interpolated-function-value="incrementInterpolated(param)" <!-- interpolatedFunctionValue({param: 'Hey'}) will call passed function with an argument. -->
                        function-item="incrementInterpolated" <!-- functionItem({param: 'Hey'})() will call passed function, but with no possibility set up a parameter. -->
                        text="'Simple text.'"  <!-- text() == 'Simple text.'-->
                        simple-value="123" <!-- simpleValue() == 123 -->
                        interpolated-value="parentScopeValue" <!-- interpolatedValue() == Some value from parent scope. -->
                        object-item="objectItem"> <!-- objectItem() == Object item from parent scope. -->
    </expression-binding>
    
All parameters will be wrapped into functions.

## Available binding through a simple sample


## Bind optional attribute
    
    bindings: {
       mandatory: '=' 
       optional: '=?',
       foo: '=?bar'
    }

Optional attributes should be marked with question mark: `=?` or `=?bar`. It is protection for `($compile:nonassign)` exception.

