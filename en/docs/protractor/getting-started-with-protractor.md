---
title: "Getting started with protractor"
slug: "getting-started-with-protractor"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing and Setting up Protractor (On Windows)
**Requirements:** Protractor requires the following dependencies to be installed prior to installation:

 - Java JDK 1.7 or higher 
 - Node.js v4 or higher
* * *

**Installation:**    
<br>Download and install Node.js from this URL: https://nodejs.org/en/

To see if the Node.js installation is successfull, you can go and check the Environment variables. The 'Path' under System Variables will be automatically updated.

[![Path variable to check Node.js installation][1]][1]

<br>You can also check the same by typing the command `npm -version` in command prompt which will give you the installed version.

[![command to check Node.js installation][2]][2]

<br>Now Protractor can be installed in two ways: Locally or Globally.

We can install protractor in a specified folder or project directory location. If we install in a project directory, every time we run, we should run from that location only.

To install locally in project directory, navigate to the project folder and type the command

 `npm install protractor`

<br>To install Protractor globally run the command:
```
$ npm install -g protractor
```
This will install two command line tools, `protractor` and `webdriver-manager`.   
Run `protractor --version` to ensure protractor was successfully installed.

`webdriver-manager` is used to download the browser driver binaries and start the selenium server.    
    Download the browser driver binaries with:
```
$ webdriver-manager update
```
Start the selenium server with:
```
$ webdriver-manager start
```
To download internet explorer driver, run the command `webdriver-manager update --ie` in command prompt.
This will download IEDriverServer.exe in your selenium folder 

  [1]: http://i.stack.imgur.com/BZCwx.png
  [2]: http://i.stack.imgur.com/yt5d7.png

## First test using Protractor
Protractor needs only two files to run the first test, spec (test code) file and configuration file. The spec file contains test code and the other one contains configuration details like spec file path, browser details, test url, framework parameters etc. To write first test we will be providing only selenium server address and spec file path.The other parameters like browser, timeout, framework will be picked up to default values.

The default browser for Protractor is Chrome.

**conf.js** - Configuration file

    exports.config = {
      seleniumAddress: 'http://localhost:4444/wd/hub',
      specs: ['spec.js']
    };

**spec.js** - Spec (test code) file

    describe('first test in protractor', function() {
      it('should verify title', function() {
        browser.get('https://angularjs.org');
    
        expect(browser.getTitle()).toEqual('AngularJS — Superheroic JavaScript MVW Framework');
      });
    });


----------


**seleniumAddress** - Path to the server where webdriver server is running .

**specs** - An array element which contains path of test files. The multiple paths can be specified by comma separated values.

**describe** - Syntax from [Jasmine][1] framework. `describe` syntax sta


  [1]: http://jasmine.github.io/2.0/introduction.html

## Write a Protractor test

Open a new command line or terminal window and create a clean folder for testing.

Protractor needs two files to run, a spec file and a configuration file.

Let's start with a simple test that navigates to the todo list example in the AngularJS website and adds a new todo item to the list.

Copy the following into `spec.js`

    
    
describe('angularjs homepage todo list', function() {
  it('should add a todo', function() {
    browser.get('https://angularjs.org');

    element(by.model('todoList.todoText')).sendKeys('write first protractor test');
    element(by.css('[value="add"]')).click();

    var todoList = element.all(by.repeater('todo in todoList.todos'));
    expect(todoList.count()).toEqual(3);
    expect(todoList.get(2).getText()).toEqual('write first protractor test');

    // You wrote your first test, cross it off the list
    todoList.get(2).element(by.css('input')).click();
    var completedAmount = element.all(by.css('.done-true'));
    expect(completedAmount.count()).toEqual(2);});});



## Selective Running Tests
Protractor can selectively run groups of tests using fdescribe() instead of describe().

    fdescribe('first group',()=>{
        it('only this test will run',()=>{
            //code that will run
        });
    });
    describe('second group',()=>{
        it('this code will not run',()=>{
            //code that won't run
        });
    });

Protractor can selectively run tests within groups using fit() instead of it().

    describe('first group',()=>{
        fit('only this test will run',()=>{
            //code that will run
        });
        it('this code will not run',()=>{
            //code that won't run
        });
    });

If there is no fit() within an fdescribe(), then every it() will run. However, a fit() will block it() calls within the same describe() or fdescribe().

    fdescribe('first group',()=>{
        fit('only this test will run',()=>{
            //code that will run
        });
        it('this code will not run',()=>{
            //code that won't run
        });
    });

Even if a fit() is in a describe() instead of an fdescribe(), it will run. Also, any it() within an fdescribe() that does not contain a fit() will run.

    fdescribe('first group',()=>{
        it('this test will run',()=>{
            //code that will run
        });
        it('this test will also run',()=>{
            //code that will also run
        });
    });
    describe('second group',()=>{
        it('this code will not run',()=>{
            //code that won't run
        });
        fit('this code will run',(){
            //code that will run
        });
    });



## Pending Tests
Protractor allows tests to be set as pending. This means that protractor will not execute the test, but will instead output:

    Pending:
    1) Test Name
    Temporarily disabled with xit
Or, if disabled with xdescribe():

    Pending:
    1) Test Name
    No reason given

### Combinations
* A xit() within an xdescribe() will output the xit() response.
* A xit() within an fdescribe() will still be treated as pending.
* A fit() within an xdescribe() will still run, and no pending tests will output anything.

## Protractor: E2E Testing for Enterprise Angular Applications
**Protractor Installation and Setup**

**Step 1**: Download and install NodeJS from here. Make sure you have latest version of node. Here, I am using node v7.8.0. You will need to have the Java Development Kit(JDK) installed to run selenium.

**Step 2**: Open your terminal and type in the following command to install protractor globally.

    npm install -g protractor

 This will install two tools such as protractor and webdriver manager.
You can verify your Protractor Installation by following command:`protractor –version.`
If Protractor is installed successfully then the system will display the installed version.(i.e. Version 5.1.1).Otherwise you will have to recheck the installation.
Step 3: Update the webdriver manager to download the necessary binaries.

    webdriver-manager update

Step 4: Following command will start up a Selenium Server. This step will run the web driver manager in the background and will listen to any tests which runs via protractor.

webdriver-manager start
You can see information about the status of the server at
 `http://localhost:4444/wd/hub/static/resource/hub.html.`

Writing First Test case using Protractor:

Before jump into the writing the test case, we have to prepare two files that is configuration file and spec file.

  In configuration file :
    
    //In conf.js
    exports.config = {
        baseUrl: ‘http://localhost:8800/adminapp’,
        seleniumAddress: ‘http://localhost:4444/wd/hub',
        specs: [‘product/product_test.js’],
        directConnect : true,
        capabilities :{
            browserName: ‘chrome’
        }
    }
    

Basic Understanding of the Terminologies used in configuration file:

**baseUrl** – A base URL for your application under test.

**seleniumAddress** – To connect to a Selenium Server which is already running.

**specs** – Location of your spec file

**directConnect** : true – To connect directly to the browser Drivers.

**capabilities** – If you are testing on a single browser, use the capabilities option. If you are testing on multiple browsers, use the multiCapabilities array.

You can find more configuration option from [here][1]. They have described all possible terminology with its definition.

In Spec file :

    //In product_test.js
    
        describe(‘Angular Enterprise Boilerplate’, function() {
          it('should have a title', function() {
            browser.get('http://localhost:8800/adminapp’);
            expect(browser.getTitle()).toEqual(‘Angular Enterprise Boilerplate’);
          });
        });

Basic Understanding of the Terminologies used in spec file:

By default,Protractor uses the jasmine framework for its testing interface. ‘describe’ and ‘it’ syntax is from jasmine framework. You can learn more from here.
Running First Test case:

Before run the test case make sure that your webdriver manager and your application running in different tabs of your terminal.

Now, Run the test with :

    Protractor app/conf.js

You should see the chrome browser opens up with your application url and close itself. The test output should be 1 tests, 1 assertion, 0 failures.

Bravo! You successfully run your first test case.


  [1]: https://github.com/angular/protractor/blob/master/lib/config.ts

