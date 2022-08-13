---
title: "Use ConfigSluper (instead of property files)"
slug: "use-configsluper-instead-of-property-files"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

ConfigSlurper allows you to use another groovy script as a config file for your script instead of using, for example, a .properties file.
You can do interesting configurations with typed properties and you don't need to convert from string. You can use lists, maps or a value based on some calculation or closure. 

## ConfigSlurper using string, number, boolean or list
In the file myConfig.groovy is the following content.

    message = 'Hello World!'
    aNumber=42
    aBoolean=false
    aList=["apples", "grapes", "oranges"]

Then in your main script you create a [ConfigSlurper][1] for your myConfig.groovy [file][2] which is really just another groovy script.

    config = new ConfigSlurper().parse(new File('/path/to/myConfig.groovy').toURL())

Then to use the items from the config you can just refer to them.

    assert 'Hello World!' == config.message
    assert 42 == config.aNumber
    assert false == config.aBoolean
    assert ["apples", "grapes", "oranges"] == config.aList

  [1]: http://docs.groovy-lang.org/latest/html/gapi/groovy/util/ConfigSlurper.html
  [2]: http://docs.groovy-lang.org/latest/html/groovy-jdk/java/io/File.html

