---
title: "Basic Maya Commands Explained"
slug: "basic-maya-commands-explained"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## What Is set/get Attr
**setAttr**

Basically as any other language setAttr can set a value for a specified attribute of a node or any context. And it support very wide range of options. For detailed instructions please visit the official documentation from maya itself [here][1].

Here is a very minimal example of setAttr

    nodeName = "pSphere1"
    cmds.setAttr("%s.tx" % nodeName, 10)

**getAttr**
Same as setAttr here it will give back the value from a specific attribute from a node. And it can return multiple types of dataTypes also. Autodesk has well documented the command [here][2]

Here is a very minimal example of getAttr

    nodeName = "pSphere1"
    txValue = cmds.getAttr("%s.tx" % nodeName)

  [1]: http://help.autodesk.com/cloudhelp/2015/ENU/Maya-Tech-Docs/CommandsPython/setAttr.html
  [2]: http://help.autodesk.com/cloudhelp/2015/ENU/Maya-Tech-Docs/CommandsPython/getAttr.html

## Basic maya command syntax
Maya commands come in a very small range of forms.  Recognizing the form that a command takes is useful for working with new commands.

Simple commands
---------------

The most basic form is simply   `<command>(<object>)` where <command> is the function you're calling and <object> is the string name of an object you are working with:

      cmds.hide('pCube1')
      cmds.delete('nurbsCurve8')

Many commands can accept multiple targets.  You can pass these individually or as iterables (lists, tuples) 

      cmds.select("top", "side")
      cameras = ['top', 'side']
      cmds.select(cams)

You can Python's [star *args](https://www.wikiod.com/python/args-and-kwargs#Using *args when writing functions) to pass an iterable object like a generator to a command:
     
      cmds.select(*a_generator_function())

A lot of commands take flags which control their behavior.  for example

      cmds.ls(type='mesh')

will return a list of meshes,  and

      cmds.ls(type='nurbsCurve')
    
returns a list of nurbs curves.

Commands which take flag can use the Python **kwargs syntax, allowing you to create dictionary of flag-value pairs and pass that to the command: 

       options = {type: 'mesh'}
       cmds.ls(**options)

is the same as 

       cmds.ls(type='mesh')

This can be very useful when assembling a command from a list of options supplied by a user or by script logic



