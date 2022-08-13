---
title: "Getting started with Angular"
slug: "getting-started-with-angular"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Angular using angular-cli
This example is a quick setup of Angular and how to generate a quick example project.

# Prerequisites:

 - [Node.js 6.9.0][1] or greater.
 - [npm][2] v3 or greater or [yarn](https://yarnpkg.com).
 - [Typings][typings] v1 or greater.

Open a terminal and run the commands one by one:


`npm install -g typings` or `yarn global add typings`

`npm install -g @angular/cli` or `yarn global add @angular/cli`

The first command installs the [typings library][typings] globally (and adds the `typings` executable to PATH). The second installs **@angular/cli** globally, adding the executable `ng` to PATH.

# To setup a new project
Navigate with the terminal to a folder where you want to set up the new project.

Run the commands:

    ng new PROJECT_NAME
    cd PROJECT_NAME
    ng serve

That is it, you now have a simple example project made with Angular. You can now navigate to the link displayed in terminal and see what it is running.

# To add to an existing project
Navigate to the root of your current project.

Run the command:

    ng init

This will add the necessary scaffolding to your project. The files will be created in the current directory so be sure to run this in an empty directory.

# Running The Project Locally

In order to see and interact with your application while it's running in the browser you must start a local development server hosting the files for your project. 

    ng serve

If the server started successfully it should display an address at which the server is running. Usually is this:

    http://localhost:4200

Out of the box this local development server is hooked up with Hot Module Reloading, so any changes to the html, typescript, or css, will trigger the browser to be automatically reloaded (but can be disabled if desired).

# Generating Components, Directives, Pipes and Services

The `ng generate <scaffold-type> <name>` (or simply `ng g <scaffold-type> <name>`) command allows you to automatically generate Angular components:

<!-- language: lang-bash -->

    # The command below will generate a component in the folder you are currently at
    ng generate component my-generated-component
    # Using the alias (same outcome as above)
    ng g component my-generated-component
    # You can add --flat if you don't want to create new folder for a component
    ng g component my-generated-component --flat
    # You can add --spec false if you don't want a test file to be generated (my-generated-component.spec.ts)
    ng g component my-generated-component --spec false
There are several possible types of scaffolds angular-cli can generate:

Scaffold Type  | Usage
---       | ---
Module    | `ng g module my-new-module`
Component | `ng g component my-new-component`
Directive | `ng g directive my-new-directive`
Pipe      | `ng g pipe my-new-pipe`
Service   | `ng g service my-new-service`
Class     | `ng g class my-new-class`
Interface | `ng g interface my-new-interface`
Enum      | `ng g enum my-new-enum`

You can also replace the type name by its first letter. For example:

`ng g m my-new-module` to generate a new module or `ng g c my-new-component` to create a component.

**Building/Bundling**

When you are all finished building your Angular web app and you would like to install it on a web server like Apache Tomcat, all you need to do is run the build command either with or without the production flag set. Production will minifiy the code and optimize for a production setting.

    ng build
or

    ng build --prod

Then look in the projects root directory for a `/dist` folder, which contains the build.

If you'd like the benefits of a smaller production bundle, you can also use Ahead-of-Time template compilation, which removes the template compiler from the final build:

    ng build --prod --aot

**Unit Testing**

Angular provides in-built unit testing, and every item created by angular-cli generates a basic unit test, that can be expended.
The unit tests are written using jasmine, and executed through Karma.
In order to start testing execute the following command:

    ng test

This command will execute all the tests in the project, and will re-execute them every time a source file changes, whether it is a test or code from the application.

For more info also visit: [angular-cli github page][3]


  [1]: https://nodejs.org
  [2]: https://www.npmjs.com/
  [3]: https://github.com/angular/angular-cli
  [typings]: https://github.com/typings/typings

## Angular "Hello World" Program
Prerequisites:
==============

**Setting up the Development Environment**

Before we get started, we have to setup our environment.

 - Install [Node.js and npm][1] if they are not already on your machine.

    Verify that you are running at least node 6.9.x and npm 3.x.x by running node -v and npm -v in a terminal/console window. Older versions produce errors, but newer versions are fine.

 - Install the [Angular CLI][2]  globally using `npm install -g @angular/cli`.


----------


## Step 1: Creating a new project ##

Open a terminal window (or Node.js command prompt in windows).

We create a new project and skeleton application using the command:

    ng new my-app

Here the `ng` is for Angular.
We get a file structure something like this.

[![File Structure_1][3]][3]

[![File Structure_2][4]][4]

There are lots of files. We need not worry about all of them now.

## Step 2: Serving the application ##

We launch our application using following command:
 

    ng serve

We may use a flag `-open`( or simply `-o`) which will automatically open our browser on `http://localhost:4200/`

    ng serve --open

Navigate browser to the address `http://localhost:4200/`. It looks something like this:

[![Welcome To App][5]][5]


## Step 3: Editing our first Angular component ##

The CLI created the default Angular component for us. This is the root component and it is named `app-root`. One can find it in `./src/app/app.component.ts`.

Open the component file and change the title property from `Welcome to app!!` to `Hello World`. The browser reloads automatically with the revised title.

Original Code : Notice the `title = 'app';`

[![Original Code][6]][6]


Modified Code : Value of `title` is changed.

[![Modified Code][7]][7]

Similarly there is a change in `./src/app/app.component.html`.

Original HTML

[![enter image description here][8]][8]

Modified HTML

[![enter image description here][9]][9]

Notice that the value of `title` from the `./src/app/app.component.ts` will be displayed. The browser reloads automatically when the changes are done. It looks something like this.

[![Hello World][10]][10]

To find more on the topic, visit this link [here][11].


  [1]: https://nodejs.org/en/download/
  [2]: https://cli.angular.io/
  [3]: https://i.stack.imgur.com/0crAT.jpg
  [4]: https://i.stack.imgur.com/zEtsK.jpg
  [5]: https://i.stack.imgur.com/Ssupw.jpg
  [6]: https://i.stack.imgur.com/fVVWw.jpg
  [7]: https://i.stack.imgur.com/ycTba.jpg
  [8]: https://i.stack.imgur.com/gwEFE.jpg
  [9]: https://i.stack.imgur.com/Sgpwj.jpg
  [10]: https://i.stack.imgur.com/4MWP9.jpg
  [11]: https://angular.io/guide/quickstart#whats-next

