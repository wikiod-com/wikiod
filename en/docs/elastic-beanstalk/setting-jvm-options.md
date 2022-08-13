---
title: "Setting JVM Options"
slug: "setting-jvm-options"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

    -bundle.zip         //--> bundle file which will be uploaded
    |--.ebextensions     //--> the file name must be exactly ".ebextensions"
       |--jvm.config    //--> config file that will set the JVM options on deployment (upload)
    |--java_app.jar     //Here, I am using Spring Boot
---
    option_settings:                                  //--> must have line
      aws:elasticbeanstalk:application:environment:   //--> namespace used to set JVM values
        XX:MaxPermSize: 256m                          //--> set XX:MaxPermSize to 256m
        Xmx: 1024m                                    //--> set Xmx to 1024m
        Xms: 512m                                     //--> set Xms to 512m

## Setting JVM Options Via Ebextensions Config
To setup JVM options inside elastic beanstalk, your bundle file must be like this:

    -bundle.zip 
    |--.ebextensions //do not forget the dot at the beginning of the name
       |--jvm.config
    |--java_app.jar

And, the *jvm.config* file must be set like this:

    option_settings:                                  
      aws:elasticbeanstalk:application:environment:   
        XX:MaxPermSize: 256m                          
        Xmx: 1024m                                    
        Xms: 512m                                     

When you upload your bundle, first those settings will be read and applied to the server's (Ec2 instance's) JVM and you must see the health as "OK".

[Source][1]


  [1]: http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options-specific.html

