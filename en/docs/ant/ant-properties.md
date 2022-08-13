---
title: "Ant Properties"
slug: "ant-properties"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Properties are key-value-pairs where Apache Ant tries to expand ${key} to value at runtime. 

Ant properties are very helpful if you have to do a lot to processing to create installables or do custom deployments etc. 

For example, you can mark ${src.dir} as source code directory,${lib.dir} as library for project,${javadoc.dir} for javadocs etc. 

Instead of writing complete path at every location you can refer them by this place holder.

## How to declare and use property in Ant.
Ant provides some built-in properties

| Property Name| Value|
| ------ | ------ |
| basedir             | the absolute path of the project's basedir|
| ant.file |the absolute path of the buildfile. |
| ant.version         | the version of Ant|
| ant.project.default-target|the name of the currently executing project's default target |
| ant.project.name | name of the project|
| ant.java.version| JVM version Ant detected|

In this example, We will create custom ant properties and use them to create a temporary directory and copy a file in it.

 1. **Properties declared within same file.**
<?xml version ="1.0"?>
    <project name="Test Project for Ant" default="init">
        <property name="temp.dir" value="${basedir}/temp" />

        <target name="init" description="initialize">
            <mkdir dir="${temp.dir}" />
            <copy file="${basedir}/test.xml" todir="${temp.dir}/" />
        </target>
    </project>

In Ant, ${basedir} will refer to the base location or the location where your ant file is present.
Here I declared a property named as 

> temp.dir

 which will refer to basedir/temp location.

So, we call target init it will replace the placeholder ${temp.dir} with its actual value and start executing our script. This target will create a directory named temp under base directory copy test.xml file to temp directory.

 2. **Properties declared in different files.**

In this example, We will refer properties declared in different file.  This is a sample file(app_version.xml) which contains application version. 


    <project name="Project Properties">
         <property name="app.version" value="1.0" />
    </project>

To include this file we will add the import ant task to import this file while executing ant targets.

    <import file="app_version.xml" />

Above code will look like

    <project name="Test Project for Ant" default="init">
    <import file="app_version.xml" />
    <property name="temp.dir" value="${basedir}/temp" />

    <target name="init" description="initialize">
        <mkdir dir="${temp.dir}" />
        <copy file="${basedir}/test.xml" todir="${temp.dir}/" />
        <echo message="App version is:${app.version}" />
    </target>
</project>

Once the file is imported, it can be directly accessed via property name(app.version).

I used .xml file, same use case will also work for .properties files.


