---
title: "Getting started with wildfly"
slug: "getting-started-with-wildfly"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Installing Wildfly is just a matter of unzipping the distribution into your local machine. Wildfly can be dowloaded from its [official website][1].

Once it is unzipped go in to bin directory of installation and run `standalone.sh` for Linux systems or `standalone.bat` for Windows systems to start your WildFly instance in default configurations. Once you see something like 

    13:16:12,503 INFO  [org.jboss.as] (Controller Boot Thread) WFLYSRV0025: WildFly Full 10.1.0.Final (WildFly Core 2.2.0.Final) started in 18909ms - Started 331 of 577 services (393 services are lazy, passive or on-demand)

then your brand new WildFly instance waiting to welcome you at : http://localhost:8080/

Some Linux distributions, such as Fedora, have Wildfly on its repositories and can be installed via YUM/DNF: `dnf install wildfly`. This, however, is not really recommended, as it tends to use slightly different versions of the libraries than the official distribution, which might cause problems that are hard to diagnose/fix.


  [1]: http://wildfly.org

## Running it via Docker
Wildfly, part of the JBoss umbrella of projects, can also be executed via Docker. On a machine with Docker properly configured, run: 

    $ docker run -it jboss/wildfly

Once the image is pulled, the container starts and the following line can be seen:

    09:44:49,225 INFO  [org.jboss.as] (Controller Boot Thread) WFLYSRV0025: WildFly Full 10.0.0.Final (WildFly Core 2.0.10.Final) started in 5644ms - Started 267 of 553 services (371 services are lazy, passive or on-demand)

This is an "empty" Wildfly server. On real world projects, the base image is meant to be extended so that your application in WAR/EAR packaging format, is added to it, as well as the necessary configuration changes to `standalone/configuration/standalone.xml`.

## Starting the server
Once Wildfly is installed by unzipping the distribution, it can be started by running the `standalone.sh` script on the `bin` directory:

```
$ ./bin/standalone.sh 
=========================================================================

  JBoss Bootstrap Environment

  JBOSS_HOME: /mnt/storage/tools/servers/wildfly-10.0.0.Final

  JAVA: java

  JAVA_OPTS:  -server -Xms64m -Xmx512m -XX:MetaspaceSize=96M -XX:MaxMetaspaceSize=256m -Djava.net.preferIPv4Stack=true -Djboss.modules.system.pkgs=org.jboss.byteman -Djava.awt.headless=true

=========================================================================

11:54:33,781 INFO  [org.jboss.modules] (main) JBoss Modules version 1.5.1.Final
11:54:34,096 INFO  [org.jboss.msc] (main) JBoss MSC version 1.2.6.Final
11:54:34,193 INFO  [org.jboss.as] (MSC service thread 1-6) WFLYSRV0049: WildFly Full 10.0.0.Final (WildFly Core 2.0.10.Final) starting
...
...
11:54:37,653 INFO  [org.jboss.as] (Controller Boot Thread) WFLYSRV0025: WildFly Full 10.0.0.Final (WildFly Core 2.0.10.Final) started in 4357ms - Started 273 of 559 services (374 services are lazy, passive or on-demand)
```

With no arguments, the default configuration is used. To override the default configuration you can providing arguments on the command line.


    --admin-only                        Set the server's running type to
                                        ADMIN_ONLY causing it to open
                                        administrative interfaces and accept
                                        management requests but not start other
                                        runtime services or accept end user
                                        requests.


    -b <value>, -b=<value>              Set system property jboss.bind.address
                                        to the given value


    -b<interface>=<value>               Set system property
                                        jboss.bind.address.<interface> to the
                                        given value


    -c <config>, -c=<config>            Name of the server configuration file
                                        to use (default is "standalone.xml")
                                        (Same as --server-config)


    --debug [<port>]                    Activate debug mode with an optional
                                        argument to specify the port. Only
                                        works if the launch script supports it.


    -D<name>[=<value>]                  Set a system property


    -h, --help                          Display this message and exit


    --read-only-server-config=<config>  Name of the server configuration file
                                        to use. This differs from
                                        '--server-config' and '-c' in that the
                                        original file is never overwritten.


    -P <url>, -P=<url>,                 Load system properties from the given
         --properties=<url>             url


    -S<name>[=<value>]                  Set a security property


    --server-config=<config>            Name of the server configuration file
                                        to use (default is "standalone.xml")
                                        (Same as -c)


    -u <value>, -u=<value>              Set system property
                                        jboss.default.multicast.address to the
                                        given value


    -v, -V, --version                   Print version and exit


    -secmgr                             Runs the server with a security manager
                                        installed.

