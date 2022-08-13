---
title: "Acceptance Testing (with Nightwatch)"
slug: "acceptance-testing-with-nightwatch"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Nightwatch has been providing Acceptance and End-to-End testing for Meteor apps since v0.5 days, and has managed migrations from PHP to Spark to Blaze and to React; and all major Continuous Integration platforms.  For additional help, please see:  

[Nightwatch API Documentation](http://nightwatchjs.org/)  
[Nightwatch.js Google Group](https://groups.google.com/forum/#!forum/nightwatchjs)  

## App Surface Area
At it's most basic level, acceptance testing is essentially black-box testing, which is fundamentally concerned with testing inputs and outputs of a closed system. As such, there are three essential features to acceptance testing: locating a resource, reading data, and writing data. When it comes to browsers and webapps, these three features basically boil down to the following:

1.  Load a webpage or application view  
2.  Inspect user interface elements (i.e. DOM)  
3.  Trigger an event / simulate a user interaction  

We call this the surface area of the application. Surface area is anything that a user sees or experiences. It's the outside of a blackbox system. And since users interact with modern web applications on video screens using web browsers, our surface coverage is defined by universal resource locators (URLs) and viewports. And so our very first walkthrough starts off looking something like the following:

<!-- language: lang-js -->
```
module.exports = {
  "Hello World" : function (client) {
    client
      // the location of our Meteor app
      .url("http://localhost:3000")

      // the size of the viewport 
      .resizeWindow(1024, 768)

      // test app output
      .verify.elementPresent('h1')
      .verify.containsText('h1', "Welcome to Meteor!")
      .verify.containsText('p', "You've pressed the button 0 times")
      .verify.elementPresent('button')

      // simulate user input
      .click('button').pause(500)

      // test app output again, to make sure input worked
      .verify.containsText('p', "button 1 times")

      // saving a copy of our viewport pixel grid
      .saveScreenshot('tests/nightwatch/screenshots/homepage.png')
      .end();
  }
};
```

## Custom Commands
Nightwatch supports creating custom commands that can simulating keystrokes, mouse clicks, and other inputs.  A custom command can be chained with other Nightwatch commands, like so:

<!-- language: lang-js -->
```
module.exports = {
  "Login App" : function (client) {
    client
      .url("http://localhost:3000")
      .login("janedoe@somewhere.com", "janedoe123")
      .end();
  }
};
```

To enable this, define a command in `./tests/nightwatch/commands/login` like so:
<!-- language: lang-js -->
```
exports.command = function(username, password) {

  this
    .verify.elementPresent('#login')

      // we clear the input in case there's any data remaining from previous visits
      .clearValue("#emailInput")
      .clearValue("#passwordInput")

      // we simulate key presses
      .setValue("#emailInput", username)
      .setValue("#passwordInput", password)

    // and we simulate a mouse click
    .click("#signInToAppButton").pause(1000)

  return this; // allows the command to be chained.
};
```

To make this all work, you will need to add `id` attributes to your login page.  At some level, it will need to roughly look something like the following:

<!-- language: lang-blaze -->
```
<template name="login">
  <div id="login">
    <input id="emailInput" name="email" type="email" />
    <input id="passwordInput" name="password" type="password" />
    <button id="#signInToAppButton">Sign In</button>
  </div>
</template>
```



## Inspecting Meteor Objects on the Client
Since Nightwatch has access to the browser console, it's possible to inspect client side objects using the `.execute()` API.  In the following example, we're checking the Session object for a particular session variable.  First, we begin by creating the file  `./tests/nightwatch/api/meteor/checkSession`, where we will keep the following command:

<!-- language: lang-js -->
```
// syncrhonous version; only works for checking javascript objects on client
exports.command = function(sessionVarName, expectedValue) {
  var client = this;
  this
    .execute(function(data){
      return Session.get(data);
    }, [sessionVarName], function(result){
      client.assert.ok(result.value);
      if(expectedValue){
        client.assert.equal(result.value, expectedValue);
      }
    })
    return this;
};
```

We can then chain it like so:
<!-- language: lang-js -->
```
module.exports = {
  "Check Client Session" : function (client) {
    client
      .url("http://localhost:3000")
      .checkSession("currentUser", "Jane Doe")
      .end();
  }
};
```

## Forms & Input Types
To upload a file, you'll first need to create a /data directory, and add the file you'll want to upload.

```
tests/nightwatch/data/IM-0001-1001.dcm
```

Your form will need an input with type of file. (Some people don't like the styling options this input provides; and a common pattern is to make this input hidden; and to have another button on the page click it on behalf of the user.)

<!-- language: lang-handlebars -->
```
<form id="myform">
    <input type="file" id="fileUpload">
    <input type="text" name="first_name">
    <input type="text" name="last_name">

    <input type="date" name="dob_month">
    <input type="date" name="dob_day">
    <input type="date" name="dob_year">

    <input type="radio" name="gender" value="M">
    <input type="radio" name="gender" value="F">
    <input type="radio" name="gender" value="O">

    <input type="select" name="hs_graduation_year">
    <input type="text" name="city">
    <input type="select" name="state">

    <input type="submit" name="submit" value="Submit">
</form>
```


Your tests will then need to use setValue() and resolve the path to the local file asset.

<!-- language: lang-js -->
```
module.exports = {
  "Upload Study" : function (client) {
    console.log(require('path').resolve(__dirname +  '/../data' ));

    var stringArray = "Chicago";

    client
      .url(client.globals.url)
      .verify.elementPresent("form#myform")

      // input[type="file"]
      .verify.elementPresent("input#fileUpload")
      .setValue('input#fileUpload', require('path').resolve(__dirname + '/../data/IM-0001-1001.dcm'))

      // input[type="text"]
      .setValue('input[name="first_name"]', 'First')
      .setValue('input[name="last_name"]', 'Last')

      // input[type="date"]
      .click('select[name="dob_month"] option[value="3"]')
      .click('select[name="dob_day"] option[value="18"]')
      .click('select[name="dob_year"] option[value="1987"]')

      // input[type="radio"]
      .click('input[name="gender"][value="M"]')

      // input[type="number"]
      .click('select[name="hs_graduation_year"] option[value="2002"]')

      // input[type="text"]
      // sometimes Nightwatch will send text faster than the browser can handle
      // which will cause skipping of letters.  In such cases, we need to slow
      // Nightwatch down; which we do by splitting our input into an array
      // and adding short 50ms pauses between each letter
      for(var i=0; i < userIdArray.length; i++) {
        client.setValue('input[name="city"]', stringArray[i]).pause(50)
      }

      // input[type="select"]
      // after an array input above, we need to resume our method chain...
      client.click('select[name="state"] option[value="CA"]')

      // input[type="number"]
      .setValue('input[name="zip"]', '01234')

      //input [ type="submit" ]
      .click('button[type="submit"]')
      .end();
  }
};
```
Credit to [Daniel Rinehart](https://groups.google.com/forum/#!searchin/nightwatchjs/radio|sort:relevance/nightwatchjs/PV-zaHLRtsA/2nbZ8v1ud84J) for inpsiring this example.


## Components & Page Objects
Page Objects are similar to Custom Commands; except they are collections of custom commands that are associated with a specific UI component.  This works extremely well with modern component based design, such as in React.

<!-- language: lang-js -->
```
module.exports = {
  url: 'http://localhost:3000/login',
  commands: [{
  login: function(email, password) {
    return this
      .clearValue('input[name="emailAddress"]')
      .clearValue('input[name="password"]')

      .setValue('input[name="emailAddress"]', email)
      .setValue('input[name="password"]', password)

      .verify.elementPresent('#loginButton')
      .click("#loginButton");
  },
  clear: function() {
    return this
      .waitForElementVisible('@emailInput')
      .clearValue('@emailInput')
      .clearValue('@passInput')
      .waitForElementVisible('@loginButton')
      .click('@loginButton')
  },
  checkElementsRendered: function(){
    return this
      .verify.elementPresent("#loginPage")
      .verify.elementPresent('input[name="emailAddress"]')
      .verify.elementPresent('input[name="password"]')
  },
  pause: function(time, client) {
    client.pause(time);
    return this;
  },
  saveScreenshot: function(path, client){
    client.saveScreenshot(path);
    return this;
  }
}],
  elements: {
    emailInput: {
      selector: 'input[name=email]'
    },
    passInput: {
      selector: 'input[name=password]'
    },
    loginButton: {
      selector: 'button[type=submit]'
    }
  }
};
```

The only caveat with using the PageObject pattern in testing components, is that the implementation breaks the method chaining flow that the native Nightwatch `verify.elementPresent` provides.  Instead, you'll need to assign the page object to a variable, and instantiate a new method chain for each page.  A reasonable price to pay for a consistent and reliable pattern for testing code reuse.

<!-- language: lang-js -->
```
module.exports = {
  tags: ['accounts', 'passwords', 'users', 'entry'],
  'User can sign up.': function (client) {

    const signupPage = client.page.signupPage();
    const indexPage = client.page.indexPage();

    client.page.signupPage()
      .navigate()
      .checkElementsRendered()
      .signup('Alice', 'Doe', 'alice@test.org', 'alicedoe')
      .pause(1500, client);

    indexPage.expect.element('#indexPage').to.be.present;
    indexPage.expect.element('#authenticatedUsername').text.to.contain('Alice Doe');
  },
}
```

