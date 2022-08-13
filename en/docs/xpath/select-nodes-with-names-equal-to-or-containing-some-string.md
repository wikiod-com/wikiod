---
title: "Select nodes with names equal to or containing some string"
slug: "select-nodes-with-names-equal-to-or-containing-some-string"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 1. Inside a specific node:

     {path-to-parent}/name()='search string']

 2. Anywhere in the document:

    //*[name()='search string']

## Parameters
| function| return value|
| ------ | ------ |
| local-name()| the node's name without prefix  |

local-name() result does not include prefix (lookup name() XPATH function for it)

## Search for nodes with name as Light, Device or Sensor
XML

    <Galaxy>
        <Light>sun</Light>
        <Device>satellite</Device>
        <Sensor>human</Sensor>
        <Name>Milky Way</Name>
    </Galaxy>

XPATH

    /Galaxy/*[local-name()='Light' or local-name()='Device' or local-name()='Sensor']
    
   or

    //*[local-name()='Light' or local-name()='Device' or local-name()='Sensor']

OUTPUT

    <Light>sun</Light>
    <Device>satellite</Device>
    <Sensor>human</Sensor>

## Search for nodes that has name that starts with Star
XML

    <College>
        <FootBall>
            <Members>20</Members>
            <Coach>Archie Theron</Coach>
            <Name>Wild cats</Name>
            <StarFootballer>David Perry</StarFootballer>
        </FootBall>
        <Academics>
            <Members>100</Members>
            <Teacher>Tim Jose</Teacher>
            <Class>VII</Class>
            <StarPerformer>Lindsay Rowen</StarPerformer>
        </Academics>
    </College>

XPATH

    /College/*/*[starts-with(local-name(),"Star")]

or 

    //*[starts-with(local-name(),"Star")]

OUTPUT

    <StarFootballer>David Perry</StarFootballer>
    <StarPerformer>Lindsay Rowen</StarPerformer>

## Search for nodes with name light (case insensitive)
XML

    <Galaxy>
        <Light>sun</Light>
        <Device>satellite</Device>
        <Sensor>human</Sensor>
        <Name>Milky Way</Name>
    </Galaxy>

XPATH

    /Galaxy/*[lower-case(local-name())="light"]
    
   or
    
    //*[lower-case(local-name())="light"]

OUTPUT

    <Light>sun</Light>

## Search for nodes that has name that contains Light
XML

    <Data>
        <BioLight>
            <name>Firefly</name>
            <model>Insect</model>
        </BioLight>
        <ArtificialLight>
            <name>Fire</name>
            <model>Natural element</model>
            <source>flint</source>
        </ArtificialLight>
        <SolarLight>
            <name>Sun</name>
            <model>Star</model>
            <source>helium</source>
        </SolarLight>
    </Data>

XPATH

    /Data/*[contains(local-name(),"Light")]
    
   or 

    //*[contains(local-name(),"Light")]

OUTPUT

    <BioLight>
      <name>Firefly</name>
      <model>Insect</model>
    </BioLight>
    <ArtificialLight>
      <name>Fire</name>
      <model>Natural element</model>
      <source>flint</source>
    </ArtificialLight>
    <SolarLight>
      <name>Sun</name>
      <model>Star</model>
      <source>helium</source>
    </SolarLight>
    

## Search for nodes that has name that ends with Ball
XML

    <College>
        <FootBall>
            <Members>20</Members>
            <Coach>Archie Theron</Coach>
            <Name>Wild cats</Name>
            <StarPlayer>David Perry</StarPlayer>
        </FootBall>
        <VolleyBall>
            <Members>24</Members>
            <Coach>Tim Jose</Coach>
            <Name>Avengers</Name>
            <StarPlayer>Lindsay Rowen</StarPlayer>
        </VolleyBall>
        <FoosBall>
            <Members>22</Members>
            <Coach>Rahul Mehra</Coach>
            <Name>Playerz</Name>
            <StarPlayer>Amanda Ren</StarPlayer>
        </FoosBall>
    </College>

XPATH

    /College/*[ends-with(local-name(),"Ball")]

   or

    //*[ends-with(local-name(),"Ball")]

OUTPUT

    <FootBall>
      <Members>20</Members>
      <Coach>Archie Theron</Coach>
      <Name>Wild cats</Name>
      <StarPlayer>David Perry</StarPlayer>
    </FootBall>
    <VolleyBall>
      <Members>24</Members>
      <Coach>Tim Jose</Coach>
      <Name>Avengers</Name>
      <StarPlayer>Lindsay Rowen</StarPlayer>
    </VolleyBall>
    <FoosBall>
      <Members>22</Members>
      <Coach>Rahul Mehra</Coach>
      <Name>Playerz</Name>
      <StarPlayer>Amanda Ren</StarPlayer>
    </FoosBall>

