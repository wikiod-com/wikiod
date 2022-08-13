---
title: "Working with knockout foreach binding with JSON"
slug: "working-with-knockout-foreach-binding-with-json"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Working with nested looping
Here is the JSON Structure we are going to use.

    {
      "employees": [
        {
          "firstName": "John",
          "lastName": "Doe",
          "skills": [
            {
              "name": "javascript",
              "rating": 5
            }
          ]
        },
        {
          "firstName": "Anna",
          "lastName": "Smith",
          "skills": [
            {
              "name": "css",
              "rating": 5
            },
            {
              "name": "javascript",
              "rating": 5
            }
          ]
        },
        {
          "firstName": "Peter",
          "lastName": "Jones",
          "skills": [
            {
              "name": "html",
              "rating": 5
            },
            {
              "name": "javascript",
              "rating": 3
            }
          ]
        }
      ]
    };

This json structure can be assigned to a variable or it can be a response of any api.

As we can see in this JSON there is an outer node employees which holds information about them, and there is an internal node which tells about each employee skills.

so here we are going to create a nested for each using knockout foreach. Here is the html

    <ul id="employee" data-bind="foreach: employee">
       <li data-bind="text:firstName + ' ' + lastName">
       </li>
       <ul data-bind="foreach : skills">
            <li data-bind="text: name">
            </li>
          <ul>
            <li>
              Rating : <!-- ko text: rating --><!-- /ko -->
            </li>
          </ul>
       </ul>
    </ul>

 Here in the above html there two list that hold foreach loop. outer loop will hold the outer node of the json structure that is employees. Inner loop holds the skills of each employee. Inside each loop we can access the properties of corresponding node. For an example we can access name and rating inside skill loop not from outside.

Below is the javascript code.


    var employeeViewModel = function(){
      var self = this;
      self.employee = ko.observableArray(employees);//here we can assign json
    }
    var viewModel = new employeeViewModel();
    ko.applyBindings(viewModel);

Form the javascript point of view there is not much code. we can directly assign our json to an observablearray which will be used by Html.



