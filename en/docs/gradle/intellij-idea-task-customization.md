---
title: "IntelliJ IDEA Task Customization"
slug: "intellij-idea-task-customization"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
 - groovy.util.Node = node.find { childNode -> return true || false }
 - node.append(nodeYouWantAsAChild)
 - groovy.util.Node parsedNode = (new XmlParser()).parseText(someRawXMLString)
 - ''' mutli-line string (not interpolated) '''
       

The three basic files of an IntelliJ project - the ipr, iws, and iml files - can be accessed as in gradle in the idea task through
    
    project.ipr
    module.iml
    workspace.iws

using the .withXml lets you access the xml. Using the .asNode() on that turns it into a groovy xml node.

Ex:

    project.ipr.withXml { provider ->
        def node = provider.asNode()

From there it's pretty simple - to modify gradle to configure IntelliJ projects for you, take the file as it starts, perform the actions you'd like gradle to take (inside IntelliJ), and then diff the new file with the old file. You should see what XML you'll need to customize the idea job. You'll also need to take note of where in the xml it's located.

One other thing to consider is that you don't want duplicate nodes within the IntelliJ files if you run the gradle idea multiple times. So, you'll want to search for the node you'd like to make and if it's not there, you can create and insert it.

**Pitfalls:**

Sometimes, when using == for string comparison in the find method, it fails. When testing and I find that to be the case, I use .contains.

When searching for nodes, not all nodes have the attribute you're using as a criteria, so be sure to check for null.



## Add a Basic Run Configuration
Assumptions for this example:
* You have a class, `foo.bar.Baz`.
* You'd like to create a run configuration that runs the main method.
* It's in a module called `fooBar`.

In your gradle file:

    idea {    
        workspace.iws.withXml { provider ->
            // I'm not actually sure why this is necessary
            def node = provider.asNode()    
    
            def runManager = node.find { it.@name.contains('RunManager')}

            // find a run configuration if it' there already
            def runner = runManager.find { it.find ({ mainClass ->
                return mainClass.@name != null && mainClass.@name == "MAIN_CLASS_NAME" && mainClass.@value != null && mainClass.@value.contains('Baz');
            }) != null }

            // create and append the run configuration if it doesn't already exists
            if (runManager != null && runner == null){
                def runnerText = '''
                    <configuration default="false" name="Baz" type="Application" factoryName="Application" nameIsGenerated="true">
                      <extension name="coverage" enabled="false" merge="false" runner="idea">
                        <pattern>
                          <option name="PATTERN" value="foo.bar.Baz" />
                          <option name="ENABLED" value="true" />
                        </pattern>
                      </extension>
                      <option name="MAIN_CLASS_NAME" value="foo.bar.Baz" />
                      <option name="VM_PARAMETERS" value="" />
                      <option name="PROGRAM_PARAMETERS" value="" />
                      <option name="WORKING_DIRECTORY" value="file://$PROJECT_DIR$" />
                      <option name="ALTERNATIVE_JRE_PATH_ENABLED" value="false" />
                      <option name="ALTERNATIVE_JRE_PATH" />
                      <option name="ENABLE_SWING_INSPECTOR" value="false" />
                      <option name="ENV_VARIABLES" />
                      <option name="PASS_PARENT_ENVS" value="true" />
                      <module name="foobar" />
                      <envs />
                      <method />
                    </configuration>'''
                runner = (new XmlParser()).parseText(runnerText)
                runManager.append(config);
            }

            // If there is no active run configuration, set the newly made one to be it
            if (runManager != null && runManager.@selected == null) {
                runManager.@selected="${runner.@factoryName}.${runner.@name}"
            }
        }
    }

