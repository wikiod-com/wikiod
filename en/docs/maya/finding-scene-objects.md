---
title: "Finding scene objects"
slug: "finding-scene-objects"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Find objects by name
Use the `ls()` commands to find objects by name:

    freds = cmds.ls("fred") 
    #finds all objects in the scene named exactly 'fred', ie [u'fred', u'|group1|fred']

Use `*` as a wildcard:

    freds = cmds.ls("fred*")
    # finds all objects whose name starts with 'fred'
    # [u'fred', u'frederick', u'fred2']

    has_fred = cmds.ls("*fred*")
    # [u'fred', u'alfred', u'fredericka']
    
ls() takes multiple filter string arguments:

    cmds.ls("fred", "barney") 
    # [u'fred', u'|group1|barney']

It can also accept an iterable argument:

    look_for = ['fred', 'barney']
    # cmds.ls(look_for)
    # [u'fred', u'|group1|barney']




## Dealing with ls() results
Using ls() as a filter can sometimes provide produce odd results.  If you accidentally forget to pass a filter argument and call `ls()` with no arguments, you will get a list of **every node in the Maya scene**:
   
     cmds.ls()
     # [u'time1', u'sequenceManager1', u'hardwareRenderingGlobals', u'renderPartition'...] etc

A common cause of this is using *args inside `ls()`:

    cmds.ls(["fred", "barney"]) # OK, returns ['fred', 'barney']
    cmds.ls([]) # OK, returns [] 
    cmds.ls(*[]) # not ok: returns all nodes!

Maya 2015 and and earlier
=======================

In Maya 2015 and earlier, an `ls()` query which finds nothing will return `None` instead of an empty list. In case of using the result, it can result in an exception:

     for item in cmds.ls("don't_exist"):
         print item
     # Error: TypeError: file <maya console> line 1: 'NoneType' object is not iterable 

The cleanest idiom for working around this is always adding an alternative output when None is returned adding `or []` after an `ls()` operation.  That will ensure that the return is an empty list rather than `None`:

     for item in cmds.ls("don't_exist") or []:
         print item
     # prints nothing since there's no result -- but no exception
    

## Finding objects by type
`ls()` includes a `type` flag, which lets you find scene nodes of a particular type.  For example:

     cameras = cmds.ls(type='camera')
     // [u'topShape', u'sideShape', u'perspShape', u'frontShape']

You can search for multiple types in the same call:

     geometry = cmds.ls(type=('mesh', 'nurbsCurve', 'nurbsSurface'))
     
You can also search for 'abstract' types, which correspond to Maya's internal class hierarchy.  These to find out what node types a particular object represents, use the `nodeType` command:

    cmds.nodeType('pCubeShape1', i=True)  # 'i' includes the inherited object types
    // Result: [u'containerBase',
     u'entity',
     u'dagNode',
     u'shape',
     u'geometryShape',
     u'deformableShape',
     u'controlPoint',
     u'surfaceShape',
     u'mesh'] //
     # in this example, ls with any of the above types will return `pCubeShape1`



## Using ls() to see if an object exists
Since `ls()` finds objects by names, it's a handy way to find out if an object is present in the scene. `ls()` with a list of objects will only return the ones which are in the scene.

     available_characters = cmds.ls('fred', 'barney', 'wilma', 'dino')
     # available_characters will contain only the named characters that are present




## Working with component selections
When working with components, such as vertices or uv points, Maya defaults to returning a colon-separated range rather than individual items:

     print cmds.ls('pCube1.vtx[*]')  # get all the vertices in the cube
     # [u'pCube1.vtx[0:7]']

You can use `ls` with the `flatten` option to force Maya to expand the range notation into individual component entries:

    expanded = cmds.ls('pCube1.vtx[*]', flatten=True)
    print expanded
    # [u'pCube1.vtx[0]', u'pCube1.vtx[1]', u'pCube1.vtx[2]', u'pCube1.vtx[3]', u'pCube1.vtx[4]', u'pCube1.vtx[5]', u'pCube1.vtx[6]', u'pCube1.vtx[7]']

This form is usually better when looping, since you don't have write any code to turn a string like `pCube1.vtx[0:7]` into multiple individual entries.

You can also get the same result using the [`filterExpand`](https://help.autodesk.com/cloudhelp/2015/CHS/Maya-Tech-Docs/Commands/filterExpand.html) command.

## Safely getting a single object from ls
Many `ls()` queries are intended to find a single object, but `ls` always returns a list (or, in older Mayas, a single  `None`).  This creates complicated error checking for a simple question.

The easiest way to get a single value from an `ls` under any circumstances is

    result = (cmds.ls('your query here') or [None])[0]

The `or` guarantees that at a minimum you'll get a list containing a single `None` so you can always index into it.  

Note that this style won't tell you if you've got more than one result -- it just makes it safe to assume a single result.



