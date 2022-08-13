---
title: "Getting started with sails.js"
slug: "getting-started-with-sailsjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new project
Once you have Sails installed, just type

    $ sails new <project_name>

This will create a skeleton Sails project in a new folder called <project_name>.

You can also create a new project in an empty folder by typing

    $ sails new

## Launch app
Once your project has been created, you can launch the app by typing

    $ sails lift

By default, you can access the app in the browser on port 1337. The URL with the port is shown in the terminal.

Another way to start the Sails app is with the `node` command:

    $ node app.js

However, you lose some development features of the `lift` command like auto-reloading of the app when assets and view files are modified.

For development you can also use:

    $ sails console

This allows you to execute command directly in command line. It's very useful for debugging Models.

## Generating sails project without frontend
If there is no need for frontend in your next project, you can run sails new with additional flag --no-frontend.

    sails new NameOfProject --no-frontend

This will generate everything needed for backend and will omit view, assets and grunt files.

More about command line and sails-new: http://sailsjs.org/documentation/reference/command-line-interface/sails-new

## Installation
**Prerequisites**

- nodejs

To install the latest stable release of sails with the command-line tool issue following command:

    $ sudo npm install sails -g

Depending on your OS you might not need to use `sudo`.


   



## Hello world
This example shows how to develop our first application step by step, assuming you already have Sails installed and a project created.

1. Create an empty controller file by typing 

```
$ sails generate controller hello
```

2. Find the new controller file at `api/controllers/HelloControllers.js` and add the `hello` method to it.

```
module.exports = {

  hello : function (req, res) {
    var myName = 'Luis';          
    return res.view('hello' , {name : myName});
    }
}
```

3. Create a new view file under the folder `views` named `hello.ejs` with the following HTML:
     
``` 
<html>
    <head></head>
    <body>
        <p>Hello {{}}.</p>
    </body>
</html>
```

4. Define a route in `config/routes.js` that calls the `hello` method in the `HelloController` controller.

```
'GET /' : 'HelloController.hello',
```

---

Now we have implemented all the code needed for this example. Let's try it:

1. Start the server
    
```
$ sails lift
```

2. Open the browser and type `http://localhost:1337`. If it's not coming up, check the URL in the `sails lift` output. The port may be different.
 
3. You should see the following output:

     Hello Luis 



