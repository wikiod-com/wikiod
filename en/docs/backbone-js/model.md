---
title: "Model"
slug: "model"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- var MyModel = Backbone.Model.extend(properties, [classProperties]); // Create a custom model
- var model = new Backbone.Model([attributes], [options]); // Instanciate a model object




## Parameters
| Parameter | Details |
| ------ | ------ |
| properties | Instance properties.   |
| classProperties | _Optional._ Properties that exist and are shared with every model instance of this type.   |
| attributes | _Optional._ Initial values of the model's `attributes`. If this parameter is left out, the model will be initialized with the values specified by the model's `defaults` property.|
| options | _Optional._ Object which serves to configure the model and is then passed to the `initialize` function.  |

## Model.urlRoot & Model.url()
By default, the `urlRoot` property is not defined. This `urlRoot` property is used by the `url` method to create a relative URL where the model's resource would be located on the server.

    var User = Backbone.Model.extend({
      
      urlRoot: '/api/users',
  
      // or
  
      urlRoot: function () {
        return '/api/users'
      }
      
    });
    
    var user = new User();

The `url` method will firstly check if the model's `idAttribute` (defaulted at 'id') has been defined. If not, the model `isNew` and `url` will simply return the results of `urlRoot`.

    user.url() // /api/users

If the model's `idAttribute` has been defined, `url` will return the `urlRoot` + the model's `idAttribute`

    user.set('id', 1);
    user.url() // /api/users/1

Calling `save` on a new model will result in a POST request to the results of `url`

    var user = new User({ username: 'johngalt' });
    user.save() // POST http://webroot/api/users

Calling `save` on an existing model will result in a PUT request to the results of `url`
    
    user.set('id', 1);
    user.set('username', 'dagnytaggart');
    user.save() // PUT http://webroot/api/users/1

Calling `fetch` on an existing model will result in a GET request to the results of `url`

    user.fetch() // GET http://webroot/api/users/1

Calling `destroy` on an existing model will result in a DELETE request to the results of `url`

    user.destroy() // DELETE http://webroot/api/users/1

## Creating models
Backbone models describe how data is stored using JavaScript objects. Each model is a hash of fields called attributes and the behaviour of the model including validation is described by options.

A model of Todo item in a TodoApp would be


    var ToDo = Backbone.Model.extend({
      defaults: {
        assignee: '',
        task: ''
      },

      validate: function(attrs) {
        var errors = {},
            hasError = false;

        if(!attrs.assignee) {
          errors.assignee = 'assignee must be set';
          hasError = true;
        }

        if(!attrs.task) {
          errors.task = 'task must be set';
          hasError = true;
        }

        if(hasError) {
          return errors;
        }
      }
    });



## Extending models
    var Vehicle = Backbone.Model.extend({
      
      description: function () {
        return 'I have ' + this.get('wheels') + ' wheels';
      }
      
    });
    
    var Bicycle = Vehicle.extend({
      
      defaults: {
        wheels: 2
      }
      
    });
    
    var Car = Vehicle.extend({
      
      defaults: {
        wheels: 4
      }
      
    });
    
    var bike = new Bicycle();
    bike.description() // I have 2 wheels;
    
    var car = new Car();
    car.description() // I have 4 wheels;;

