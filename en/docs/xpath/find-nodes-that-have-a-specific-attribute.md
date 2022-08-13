---
title: "Find nodes that have a specific attribute"
slug: "find-nodes-that-have-a-specific-attribute"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 1. Inside a specific node
    - /path to/element[@attribute_name]
 2. Anywhere in the document
    - //*[@attribute_name]
 3. Inside a specific node with some value
    - /path to/element[@attribute_name='search value']
    - /path to/element[@attribute_name="search value"]
 4. Anywhere in the document with some value
    - //*[@attribute_name='search string']
    - //*[@attribute_name="search string"]

## Parameters
| Selector| function|
| ------ | ------ |
| @attribute_name| It selects the attribute value for a node, if present|

Using [@attribute_name] we can select nodes that have the attribute irrespective of the value.

We can use any of the functions or combination of the functions such as starts-with and lowercase, for example, with this selector to suit our needs.

## Find nodes by substring matching of an attribute's value
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[contains(@name,'Ear')]

   or
    
    //*[contains(@name,'Ear')]

Double quotes can also be used in place of single quotes:

    /Galaxy/*[contains(@name, "Ear")]

OUTPUT

    <CelestialObject name="Earth" type="planet" />

## Find nodes with a specific attribute
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[@name]
    
   or
    
    //*[@name]

OUTPUT

    <CelestialObject name="Earth" type="planet" />
    <CelestialObject name="Sun" type="star" />

## Find nodes with a specific attribute value
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[@name='Sun']

   or

    //*[@name='Sun']

OUTPUT

    <CelestialObject name="Sun" type="star" />


## Find nodes by substring matching of an attribute's value (case-insensitive)
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[contains(lower-case(@name),'ear')]

   or

    //*[contains(lower-case(@name),'ear')]

   or, with the string in double quotes:

    //*[contains(lower-case(@name), "ear")]

OUTPUT

    <CelestialObject name="Earth" type="planet" />

## Find nodes by substring matching the start of an attribute's value
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[starts-with(lower-case(@name),'ear')]

   or

    //*[starts-with(lower-case(@name),'ear')]

OUTPUT

    <CelestialObject name="Earth" type="planet" />

## Find nodes by substring matching the end of an attribute's value
XML

    <Galaxy>
        <name>Milky Way</name>
        <CelestialObject name="Earth" type="planet"/>
        <CelestialObject name="Sun" type="star"/>
    </Galaxy>

XPATH

    /Galaxy/*[ends-with(lower-case(@type),'tar')]
    
   or

    //*[ends-with(lower-case(@type),'tar')]

OUTPUT

    <CelestialObject name="Sun" type="star" />

