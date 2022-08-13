---
title: "Getting started with jetty"
slug: "getting-started-with-jetty"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing and Running Jetty
For the sake of this example, it is assumed the user is running Jetty as a distribution. For information on how to run Jetty as an embedded web server, please refer to the [official documentation][1].

Jetty can be downloaded from [here][2] and is available in both .zip and .gzip formats. Current versions of Jetty require Java 1.8 to be installed, which can be obtained from [Oracle's website][3].

Extract the distribution to a directory of your choosing. In the following example, this directory will be referred to as $JETTY_HOME. To start Jetty, simply start the *start.jar* file in the distribution from the command line:

    $ cd $JETTY_HOME
    $ java -jar start.jar

Performing this from the $JETTY_HOME directory will produce an output similar to this:

    2016-07-25 09:24:46.019:INFO::main: Logging initialized @313ms
    2016-07-25 09:24:46.068:WARN:oejs.HomeBaseWarning:main: This instance of Jetty is not running from a separate {jetty.base} directory, this is not recommended.  See documentation at http://www.eclipse.org/jetty/documentation/current/startup.html
    2016-07-25 09:24:46.191:INFO:oejs.Server:main: jetty-9.3.11.v20160721
    2016-07-25 09:24:46.207:INFO:oejdp.ScanningAppProvider:main: Deployment monitor [file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/webapps/] at interval 1
    2016-07-25 09:24:46.226:INFO:oejs.AbstractConnector:main: Started ServerConnector@735f7ae5{HTTP/1.1,[http/1.1]}{0.0.0.0:8080}
    2016-07-25 09:24:46.227:INFO:oejs.Server:main: Started @521ms

The server is running, but you will notice that if you point your browser to localhost:8080, you receive a 404 error.

In the output above there is a warning about running the server from the $JETTY_HOME directory. Jetty 9.1 introduced the concept of having a Jetty Home directory and a Jetty Base directory (or directories). The basic premise is that the Jetty Home directory is the standard of truth for all server files. It contains all of the files required to run the server and the defaults for all server extensions. The Jetty Base directory (or directories) are for managing your instance(s) of Jetty and contain all of the web applications, third party plug-ins and customizations for your server. For more information, please see the official documentation on [Managing Jetty Base and Jetty Home][4].

The Jetty distribution comes with a base directory with several web applications already configured. This directory is aptly named *demo-base*:

    $ cd demo-base
    $ java -jar $JETTY_HOME/start.jar
    2016-07-25 09:25:48.435:INFO::main: Logging initialized @316ms
    2016-07-25 09:25:48.645:WARN::main: demo test-realm is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:48.647:INFO:oejs.Server:main: jetty-9.3.11.v20160721
    2016-07-25 09:25:48.662:INFO:oejdp.ScanningAppProvider:main: Deployment monitor [file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/webapps/] at interval 1
    2016-07-25 09:25:48.923:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=81ms
    2016-07-25 09:25:49.078:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@527740a2{/proxy,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-xref-proxy.war-_xref-proxy-any-1184996072304577986.dir/webapp/,AVAILABLE}{/xref-proxy.war}
    2016-07-25 09:25:49.083:INFO:oejsh.ContextHandler:main: Started o.e.j.s.h.MovedContextHandler@229d10bd{/oldContextPath,null,AVAILABLE}
    2016-07-25 09:25:49.166:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=34ms
    2016-07-25 09:25:49.168:WARN::main: test-spec webapp is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:49.238:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@647fd8ce{/test-spec,[file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test-spec.war-_test-spec-any-195142828825451628.dir/webapp/, jar:file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test-spec.war-_test-spec-any-195142828825451628.dir/webapp/WEB-INF/lib/test-web-fragment-9.3.11.v20160721.jar!/META-INF/resources],AVAILABLE}{/test-spec.war}
    2016-07-25 09:25:49.259:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=15ms
    2016-07-25 09:25:49.284:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@55b7a4e0{/,file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/webapps/ROOT/,AVAILABLE}{/ROOT}
    2016-07-25 09:25:49.323:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=16ms
    2016-07-25 09:25:49.325:WARN::main: test-jaas webapp is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:49.349:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@a514af7{/test-jaas,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test-jaas.war-_test-jaas-any-6378488515259627366.dir/webapp/,AVAILABLE}{/test-jaas.war}
    2016-07-25 09:25:49.436:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=28ms
    2016-07-25 09:25:49.438:WARN::main: test-jndi webapp is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:49.476:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@3febb011{/test-jndi,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test-jndi.war-_test-jndi-any-5492379371528358988.dir/webapp/,AVAILABLE}{/test-jndi.war}
    2016-07-25 09:25:49.553:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=38ms
    2016-07-25 09:25:49.555:WARN::main: async-rest webapp is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:49.576:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@36fc695d{/async-rest,[file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-async-rest.war-_async-rest-any-7692963153880036981.dir/webapp/, jar:file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-async-rest.war-_async-rest-any-7692963153880036981.dir/webapp/WEB-INF/lib/example-async-rest-jar-9.3.11.v20160721.jar!/META-INF/resources],AVAILABLE}{/async-rest.war}
    2016-07-25 09:25:49.669:INFO:oeja.AnnotationConfiguration:main: Scanning elapsed time=34ms
    2016-07-25 09:25:49.670:WARN::main: test webapp is deployed. DO NOT USE IN PRODUCTION!
    2016-07-25 09:25:49.913:INFO:oejsh.ManagedAttributeListener:main: update PushFilter null->org.eclipse.jetty.servlets.PushCacheFilter@4e0ae11f on o.e.j.w.WebAppContext@5dda768f{/test,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test.war-_test-any-2157501520644819622.dir/webapp/,STARTING}{/test.war}
    2016-07-25 09:25:49.921:INFO:oejsh.ManagedAttributeListener:main: update QoSFilter null->org.eclipse.jetty.servlets.QoSFilter@376a0d86 on o.e.j.w.WebAppContext@5dda768f{/test,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test.war-_test-any-2157501520644819622.dir/webapp/,STARTING}{/test.war}
    2016-07-25 09:25:49.922:WARN:oeju.DeprecationWarning:main: Using @Deprecated Class org.eclipse.jetty.servlets.MultiPartFilter
    2016-07-25 09:25:49.941:INFO:oejsh.ContextHandler:main: Started o.e.j.w.WebAppContext@5dda768f{/test,file:///private/var/folders/h6/yb_lbnnn11g0y1jjlvqg631h0000gn/T/jetty-0.0.0.0-8080-test.war-_test-any-2157501520644819622.dir/webapp/,AVAILABLE}{/test.war}
    2016-07-25 09:25:49.952:INFO:oejs.AbstractConnector:main: Started ServerConnector@74f6c5d8{HTTP/1.1,[http/1.1]}{0.0.0.0:8080}
    2016-07-25 09:25:49.957:INFO:oejus.SslContextFactory:main: x509=X509@43015c69(jetty,h=[jetty.eclipse.org],w=[]) for SslContextFactory@19b89d4(file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/etc/keystore,file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/etc/keystore)
    2016-07-25 09:25:49.958:INFO:oejus.SslContextFactory:main: x509=X509@4bbf6d0e(mykey,h=[],w=[]) for SslContextFactory@19b89d4(file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/etc/keystore,file:///Users/user/installs/repository/jetty-distribution-9.3.11.v20160721/demo-base/etc/keystore)
    2016-07-25 09:25:49.968:INFO:oejs.AbstractConnector:main: Started ServerConnector@13608c15{SSL,[ssl, http/1.1]}{0.0.0.0:8443}
    2016-07-25 09:25:49.968:INFO:oejs.Server:main: Started @1850ms

Now if you point your browser to *localhost:8080* you will see a Jetty welcome page along with several links to example web applications as well as links to the official Jetty documentation and user groups. **NOTE**: The web applications that come with the Jetty distribution are *not* designed to be secure or fully functional, they are only provided as examples. Using them in your production environments is **not** recommended.

Congratulations! You have installed and started a Jetty web server. For more instructions on how to configure your server, and for more advanced subjects, please refer to the [Official Documentation.][5]


  [1]: https://www.eclipse.org/jetty/documentation/current/embedding-jetty.html
  [2]: https://www.eclipse.org/jetty/download.html
  [3]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
  [4]: https://www.eclipse.org/jetty/documentation/current/startup-base-and-home.html
  [5]: https://www.eclipse.org/jetty/documentation/current/index.html

