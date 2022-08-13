---
title: "Web Scripts"
slug: "web-scripts"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Webscripts are functional modules in the Alfresco, which can just show some informations or also make some things inside the Alfresco (eg. run workflow, working with files, users, groups, or other entities).

Every webscripts has two main parts- code (.js, .java) and Freemaker template (.ftl)

Webscript can have also additional .properties file with text strings used in.
Parts are paired in the context.xml file (Spring framework logic).

It's binded to any URL described in this file.

## Hello World Web Script
Let's make a hello world web script. Web scripts have a descriptor, a controller, and, optionally, a view. These files must follow a naming convention.

This descriptor is named helloworld.get.desc.xml.

    <webscript>
      <shortname>Hello World</shortname>
      <description>Hello world web script</description>
      <url>/example/helloworld?name={nameArgument}</url>
      <authentication>user</authentication>  
    </webscript>

You can see that the descriptor declares that this web script will be mapped to a URL, "/example/helloworld", and that it requires user authentication. The descriptor also declares an argument called name.

Here is the controller. It is named helloworld.get.js.

    model.foo = "bar";

This controller is written in JavaScript but controllers can also be written in Java. With a bit more work you can write controllers in other languages too.

This controller doesn't do much. It just adds a new variable to the model called "foo" and gives it a value of "bar".

Your controller has access to a variety of root scoped variables which are all documented in the [official documentation][1].

Finally, let's look at the view. It's named helloworld.get.html.ftl

    <html>
      <body>
        <p>Hello, ${args.name!"name not specified"}!</p>
        <p>Foo: ${foo}</p>
      </body>
    </html>

You can see from the name that this view is implemented as a Freemarker template and outputs HTML. This view grabs the value of "foo" from the model and it also grabs the name argument that was passed in to the web script. If a name argument is not specified the template provides some default text.

If you wanted to produce XML or JSON instead you can--just change the name, then update your template implementation accordingly.

**Deployment**

Web scripts can be deployed to the classpath or uploaded to the repository. For example, to deploy this web script by uploading to the repository, follow these steps:
1. Upload these three files to Data Dictionary/Web Scripts Extensions
2. Refresh the Web Scripts by going to http://localhost:8080/alfresco/s/index and clicking "Refresh Web Scripts".
3. Navigate to the web script by going to http://localhost:8080/alfresco/s/example/helloworld?name=Jeff

  [1]: http://docs.alfresco.com/community/references/API-JS-rootscoped.html

## Folder Maker: A Web Script that handles POST
The Hello World Web Script handles GET HTTP methods. But what if you want to create data on the server? For that your web script should handle POST.

Here is a simple example that creates new folders in Company Home. It is invoked by making a POST call with a JSON body that looks like:

    {'name':'testfolder'}

Optionally, you could add a title or a description to the folder by passing those in as part of the body, like:

    {
      'name':'testfolder',
      'title':'test title',
      'description':'test description'
    }

The descriptor is called foldermaker.post.desc.xml:

    <webscript>
      <shortname>Folder Maker</shortname>
      <description>Creates folders</description>
      <family>Examples</family>
      <url>/example/folders</url>
      <format default="json"></format>
      <authentication>user</authentication>
    </webscript>

The optional "family" element is a convenient way to group web scripts in the web script index. The "format" element declares that this web script returns JSON.

The controller is called foldermaker.post.json.js:

    var name = title = desc = null;
    
    var name = json.get('name');
    
    try {
      title = json.get('title');
    } catch (err) {}
    
    try {
      desc = json.get('description');
    } catch (err) {}
    
    var folder = companyhome.createFolder(name);
    
    var needsSave = false;
    
    if (title != null) {
      folder.properties['cm:title'] = title;
      needsSave = true;
    }
    
    if (desc != null) {
      folder.properties['cm:description'] = desc;
      needsSave = true;
    }
    
    if (needsSave) {
      folder.save();
    }
    
    model.id = folder.nodeRef.toString();
    model.name = name;
    model.title = title;
    model.description = desc;

Notice that this controller has "json" in its name. This tells Alfresco to expect a JSON payload. Alfresco will automatically parse the JSON and put it in a root variable called "json".

The controller grabs the name, title, and description from the JSON and creates the folder using the root scope variable called "companyhome".

If a title or description is passed in those properties get saved.

The values that were passed in as well as the new folder's node reference get set on the model before handing control over to the view.

The view is named foldermaker.post.json.ftl:

    {
      <#if title??>
      "title": "${title}",
      </#if>
      <#if description??>
      "description": "${description}",
      </#if>
      "id": "${id}",
      "name": "${name}"
    }

This freemarker just echoes back the values set on the model as JSON. The title and description may not always be present, so the view uses the Freemarker null check built-in in an if statement to avoid returning those if they were not set.

Any HTTP client can be used to test this web script out. Here's what it would look like using curl:

    jpotts$ curl -uadmin:admin -H "content-type: application/json" -X POST "http://localhost:8080/alfresco/s/example/folders" -d "{'name':'testfolder','title':'test title', 'description':'test desc'}"
    {
      "title": "test title",
      "description": "test desc"
      "id": "workspace://SpacesStore/cc26a12f-306b-41f1-a859-668f11fc2a54",
      "name": "testfolder"
    }

Note that curl is passing in basic auth credentials. In this case it is using "admin". Because this example creates items in Company Home, you must use a user that has the appropriate permissions to do that.

This web script has no real error checking. If you don't pass in a name or you pass in a name that has already been used you will see an error.





