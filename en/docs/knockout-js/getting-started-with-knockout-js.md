---
title: "Getting started with knockout.js"
slug: "getting-started-with-knockoutjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started: Hello world!
## Creating an HTML document and enabling knockout.js

Create an HTML file and include `knockout.js` via a `<script>` tag.

    <!DOCTYPE html>
    <html>
    <head>
        <title>Hello world! (knockout.js)</title>
    </head>
    <body>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/knockout/3.4.0/knockout-debug.js"></script>
    </body>
    </html>

Add a second `<script>` tag _under_ the knockout script. In this script tag, we'll initialize a view model and apply _data binds_ to our Document.

    <script>
      var ViewModel = function() {
        this.greeting = ko.observable("Hello");
        this.name = ko.observable("World");

        this.appHeading = ko.pureComputed(function() {
          return this.greeting() + ", " + this.name() + "!";
        }, this);
      };

      var appVM = new ViewModel();

      ko.applyBindings(appVM);
    </script>

Now, continue creating a _view_ by adding some HTML to the body:

    <section>
      <h1 data-bind="text: appHeading"></h1>
      <p>Greeting: <input data-bind="textInput: greeting" /></p>
      <p>Name: <input data-bind="textInput: name" /></p>
    </section>

When the HTML document is opened and the scripts are executed, you'll see a page that says **Hello, World!**. When you change the words in the `<input>` elements, the `<h1>` text is automatically updated.

----------

## How the created file works

1. A debug version of knockout is loaded from an external source (cdnjs)

Code:

    <script src="https://cdnjs.cloudflare.com/ajax/libs/knockout/3.4.0/knockout-debug.js"></script>

2. A viewmodel instance is created that has _observable_ properties. This means knockout is able to detect changes and update the UI accordingly.

Code:

    var appVM = new ViewModel();

3. Knockout checks the DOM for `data-bind` attributes and updates the UI using the provided viewmodel.

Code:

    ko.applyBindings(appVM);

4. When it encounters a `text` binding, knockout uses the property's value as it is defined in the bound viewmodel to inject a text node:

Code: 

    <h1 data-bind="text: appHeading"></h1>
    

## Installation or Setup
Knockout is available on most JavaScript platforms, or as a standalone script.

# Include as a script
You can download the script from it's [download page][1], then include it in your page with a standard `script tag`

```javascript
<script type='text/javascript' src='knockout-3.4.0.js'></script>
```

## Using a CDN

You can also include knockout from either the Microsoft CDN, or [CDNJS][2]

```javascript
<script type='text/javascript' src='//ajax.aspnetcdn.com/ajax/knockout/knockout-3.4.0.js'></script>
```
OR
```javascript
<script type='text/javascript' src='//cdnjs.cloudflare.com/ajax/libs/knockout/3.4.0/knockout-min.js'></script>
```

# Install from npm

```
npm install knockout
```

optionally you can add the `--save` parameter to keep it in your `package.json` file

# Install from bower

```
bower install knockout
```

optionally you can add the `--save` parameter to keep it in your `bower.json` file

# Install from NuGet

```
Install-Package knockoutjs
```


  [1]: http://knockoutjs.com/downloads/index.html
  [2]: https://cdnjs.com/

## Computed Observables
Computed observables are functions that can "watch" or "react" to other observables on the view model.  The following example shows how you would display the total number of users and the average age.  

*Note: The example below can also make use of **pureComputed()** (introduced in v3.2.0) since the function simply calculates something based on other view model properties and returns a value.*

<!-- language: lang-html -->

    <div>
      Total Users: <span data-bind="text: TotalUsers">2</span><br>
      Average Age: <span data-bind="text: UsersAverageAge">32</span>
    </div>

<!-- language: lang-js-->
    var viewModel = function() {

        var self = this;

        this.Users = ko.observableArray([
            { Name: "John Doe", Age: 30 },        
            { Name: "Jane Doe", Age: 34 }
        ]);

        this.TotalUsers = ko.computed(function() {
            return self.Users().length;
        });

        this.UsersAverageAge = ko.computed(function() {
            var totalAge = 0;
            self.Users().forEach(function(user) {
                totalAge += user.Age;
            });
       
            return totalAge / self.TotalUsers();
        });
    };

    ko.applyBindings(viewModel);

