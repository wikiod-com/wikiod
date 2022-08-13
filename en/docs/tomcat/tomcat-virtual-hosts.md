---
title: "Tomcat Virtual Hosts"
slug: "tomcat-virtual-hosts"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

**Host Manager** is a web application inside of Tomcat that creates/removes *Virtual Hosts* within Tomcat.

A *Virtual Host* allows you to define multiple hostnames on a single server, so you can use the same server to handles requests to, for example, `ren.myserver.com` and `stimpy.myserver.com`.

Unfortunately documentation on the GUI side of the Host Manager doesn't appear to exist, but documentation on configuring the virtual hosts manually in `context.xml` is here: 

http://tomcat.apache.org/tomcat-7.0-doc/virtual-hosting-howto.html.

The full explanation of the `Host` parameters you can find here:

http://tomcat.apache.org/tomcat-7.0-doc/config/host.html.

**<sub>Adapted from [my][1] answer: http://stackoverflow.com/a/26248511/6340</sub>**


  [1]: http://stackoverflow.com/users/6340/brass-kazoo

## Tomcat Host Manager Web Application
Tomcat's Host Manager application by default is located at http://localhost:8080/host-manager, but is not accessible until a user is given permission in the `conf/tomcat-users.xml` file. The file needs to have: 
1. A `manager-gui` role
2. A user with this role

For example:

    <tomcat-users>
        ...
        <role rolename="manager-gui"/>
        ....
        <user username="host-admin" password="secretPassword" roles="manager-gui"/>
    </tomcat-users>





## Adding a Virtual Host via the Tomcat Host Manager Web Application
Once you have access to the host-manager, the GUI will let you add a virtual host.

> **Note:** In Tomcat 7 and 8, adding a virtual host via the GUI __does not write the vhost to config files__. You will need to manually edit
> the `server.xml` file to have the vhost available after a restart. See
> http://tomcat.apache.org/tomcat-7.0-doc/virtual-hosting-howto.html for
> further info on the `<Host>` tag in `server.xml`

[![Tomcat Host Manager - Add Virtual Host][1]][1]


At a minimum you need the `Name` and `App Base` fields defined. Tomcat will then create the following directories:

    {CATALINA_HOME}\conf\Catalina\{Name}
    {CATALINA_HOME}\{App Base}

* `App Base` will be where web applications will be deployed to the virtual host. Can be relative or absolute.
* `Name` is usually the fully-qualified domain name (e.g. `ren.myserver.com`)
* `Alias` can be used to extend the `Name` also where two addresses should resolve to the same host (e.g. `www.ren.myserver.com`). Note that this needs to be reflected in DNS records.

The checkboxes are as follows:

* `Auto Deploy`: Automatically redeploy applications placed into App Base. Dangerous for Production environments!
* `Deploy On Startup`: Automatically boot up applications under App Base when Tomcat starts
* `Deploy XML`: Determines whether to parse the application's `/META-INF/context.xml`
* `Unpack WARs`: Unpack WAR files placed or uploaded to the App Base, as opposed to running them directly from the WAR.
* **Tomcat 8** `Copy XML`: Copy an application's `META-INF/context.xml` to the App Base/XML Base on deployment, and use that exclusively, regardless of whether the application is updated. Irrelevant if `Deploy XML` is false.
* `Manager App`: Add the manager application to the Virtual Host (Useful for controlling the applications you might have underneath `ren.myserver.com`)

**<sub>Adapted from [my][2] answer: http://stackoverflow.com/a/26248511/6340</sub>**


  [2]: http://stackoverflow.com/users/6340/brass-kazoo

  [1]: http://i.stack.imgur.com/lYTrH.png

## Adding a Virtual Host to server.xml
Once a virtual host has been added via the web application, directories will exist at:

    {CATALINA_HOME}\conf\Catalina\{Name}
    {CATALINA_HOME}\{App Base}

To persist the virtual host after a restart, the `server.xml` file must be updated with the configuration. A `Host` element needs to be added inside the `Engine` element, similar to this:

    <Engine name="Catalina" ...>
      ...
      <Host name="my-virtual-app" appBase="virtualApp" autoDeploy="true" unpackWARs="true" ... />
    </Engine>

The attributes in the `Host` element should reflect the selections made in the host manager GUI (see the [Host documentation][1] for details), but can be changed. Note that the `Manager App` option in the GUI does not correspond to any `Host` attribute.


  [1]: http://tomcat.apache.org/tomcat-7.0-doc/config/host.html

