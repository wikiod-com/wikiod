---
title: "Get nodes relative to the current node"
slug: "get-nodes-relative-to-the-current-node"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
 1. All ancestors of a node
    - /path to the node/ancestor::node()
 2. A specific ancestor of a node
    - /path to the node/ancestor::ancestor_name
 3. Parent of a node
    - /path to the node/parent::node()
 4. Following siblings of a node
    - /path to the node/following-sibling::node()
 5. A specific sibling following a node
    - /path to the node/following-sibling::sibling_name
 6. Preceding siblings of a node
    - /path to the node/preceding-sibling::node()
 7. A specific sibling preceding a node
    - /path to the node/preceding-sibling::sibling_name
 8. All immediate child nodes of a node
    - /path to the node/child::node()
 9. A specific immediate child node of a node
    - /path to the node/child::chid_name
10. All the descendants of a node
    - /path to the node/descendant::node()
11. All specific descendants of a node
    - /path the to node/descendant::descendant_name

 

    

## Parameters
| Axis| selects|
| ------ | ------ |
| ancestor| all the ancestor nodes|
|parent| parent node|
|following-sibling| siblings following the node|
|preceding-sibling| siblings preceding the node|
|child|immediate children|
|descendant|all the descendant irrespective of the nesting level|

These axes can be used in combination with other functions to suit our needs.

## Find My ancestors
XML

    <GrandFather name="Bardock" gender="male" spouse="Gine">
        <Dad name="Goku" gender="male" spouse="Chi Chi">
            <Me name="Gohan" gender="male"/>
            <brother name="Goten" gender="male"/>
        </Dad>
    </GrandFather>
    
XPATH

    //Me/ancestor::node()

OUTPUT

    <GrandFather name="Bardock" gender="male" spouse="Gine">
      <Dad name="Goku" gender="male" spouse="Chi Chi">
        <Me name="Gohan" gender="male" />
        <brother name="Goten" gender="male" />
      </Dad>
    </GrandFather>
    <Dad name="Goku" gender="male" spouse="Chi Chi">
      <Me name="Gohan" gender="male" />
      <brother name="Goten" gender="male" />
    </Dad>

## Find My Parent
XML

    <GrandFather name="Bardock" gender="male" spouse="Gine">
        <Dad name="Goku" gender="male" spouse="Chi Chi">
            <Me name="Gohan" gender="male"/>
            <brother name="Goten" gender="male"/>
        </Dad>
    </GrandFather>
        
XPATH

    //Me/ancestor::Dad

   or

    //Me/parent::node()

OUTPUT

    <Dad name="Goku" gender="male" spouse="Chi Chi">
      <Me name="Gohan" gender="male" />
      <brother name="Goten" gender="male" />
    </Dad>

## Find my grand father
XML

    <GrandFather name="Bardock" gender="male" spouse="Gine">
      <Dad name="Goku" gender="male" spouse="Chi Chi">
        <Me name="Gohan" gender="male" />
        <brother name="Goten" gender="male" />
      </Dad>
    </GrandFather>
    
XPATH

    //Me/ancestor::GrandFather

   or

    //Me/parent::node()/parent::node()

OUTPUT

    <GrandFather name="Bardock" gender="male" spouse="Gine">
      <Dad name="Goku" gender="male" spouse="Chi Chi">
        <Me name="Gohan" gender="male" />
        <brother name="Goten" gender="male" />
      </Dad>
    </GrandFather>

## Find my brother
XML

    <GrandFather name="Bardock" gender="male" spouse="Gine">
      <Dad name="Goku" gender="male" spouse="Chi Chi">
        <brother name="Goten" gender="male" />
        <Me name="Gohan" gender="male" />
        <brother name="Goten" gender="male" />
      </Dad>
    </GrandFather>
    
XPATH

    //Me/following-sibling::brother

OUTPUT

    <brother name="Goten" gender="male" />

## Get all avatars before Parashurama
XML

    <Dashavatar>
        <Avatar name="Matsya"/>
        <Avatar name="Kurma"/>
        <Avatar name="Varaha"/>
        <Avatar name="Narasimha"/>
        <Avatar name="Vamana"/>
        <Avatar name="Balabhadra"/>
        <Avatar name="Parashurama"/>
        <Avatar name="Rama"/>
        <Avatar name="Krishna"/>
        <Avatar name="Kalki"/>
    </Dashavatar>
    
XPATH

    //Avatar[@name='Parashurama']/preceding-sibling::node()

OUTPUT

    <Avatar name="Matsya"/>
    <Avatar name="Kurma"/>
    <Avatar name="Varaha"/>
    <Avatar name="Narasimha"/>
    <Avatar name="Vamana"/>
    <Avatar name="Balabhadra"/>

## Get all avatars after Parashurama
XML

    <Dashavatar>
        <Avatar name="Matsya"/>
        <Avatar name="Kurma"/>
        <Avatar name="Varaha"/>
        <Avatar name="Narasimha"/>
        <Avatar name="Vamana"/>
        <Avatar name="Balabhadra"/>
        <Avatar name="Parashurama"/>
        <Avatar name="Rama"/>
        <Avatar name="Krishna"/>
        <Avatar name="Kalki"/>
    </Dashavatar>
    
XPATH

    //Avatar[@name='Parashurama']/following-sibling::node()

OUTPUT

    <Avatar name="Rama" />
    <Avatar name="Krishna" />
    <Avatar name="Kalki" />

## Get all avatars except the current one (Parusharama)
XML

    <Dashavatar>
        <Avatar name="Matsya"/>
        <Avatar name="Kurma"/>
        <Avatar name="Varaha"/>
        <Avatar name="Narasimha"/>
        <Avatar name="Vamana"/>
        <Avatar name="Balabhadra"/>
        <Avatar name="Parashurama"/>
        <Avatar name="Rama"/>
        <Avatar name="Krishna"/>
        <Avatar name="Kalki"/>
    </Dashavatar>
    
XPATH

    //Avatar[@name='Parashurama']/following-sibling::Avatar | //Avatar[@name='Parashurama']/preceding-sibling::Avatar

OUTPUT

    <Avatar name="Matsya" />
    <Avatar name="Kurma" />
    <Avatar name="Varaha" />
    <Avatar name="Narasimha" />
    <Avatar name="Vamana" />
    <Avatar name="Balabhadra" />
    <Avatar name="Rama" />
    <Avatar name="Krishna" />
    <Avatar name="Kalki" />

## Get all the details (child nodes) of House
XML

    <House>
        <Rooms>10</Rooms>
        <People>4</People>
        <TVs>4</TVs>
        <Floors>2</Floors>
    </House>

XPATH

    /House/child::node()

OUTPUT

    <Rooms>10</Rooms>
    <People>4</People>
    <TVs>4</TVs>
    <Floors>2</Floors>

## Get all rooms (immediate children named Room) in House
XML

    <House>
        <numRooms>4</numRooms>
        <Room name="living"/>
        <Room name="master bedroom"/>
        <Room name="kids' bedroom"/>
        <Room name="kitchen"/>
    </House>

XPATH

    /House/child::Room

   or

    /House/*[local-name()='Room']

OUTPUT

    <Room name="living" />
    <Room name="master bedroom" />
    <Room name="kids' bedroom" />
    <Room name="kitchen" />

## Get all rooms (irrespective of the position) in House
XML

    <House>
        <numRooms>4</numRooms>
        <Floor number="1">
            <Room name="living"/>
            <Room name="kitchen"/>
        </Floor>
        <Floor number="2">
            <Room name="master bedroom"/>
            <Room name="kids' bedroom"/>    
        </Floor>
    </House>

XPATH

    /House/descendant::Room

OUTPUT

    <Room name="living" />
    <Room name="kitchen" />
    <Room name="master bedroom" />
    <Room name="kids' bedroom" />

