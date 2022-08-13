---
title: "Get the count of nodes"
slug: "get-the-count-of-nodes"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
 - count(node-set)



## Parameters
| function | returns |
| ------ | ------ |
| count   | total number of nodes in the node set   |

We can use this in combination of other functions and axes to suit our needs.

## How many children does Goku have?
XML

    <Goku>
        <child name="Gohan"/>
        <child name="Goten"/>
    </Goku>

XPATH

    count(/Goku/child)

OUTPUT

    2.0

## How many plants are there in the house?
XML

    <House>
        <LivingRoom>
            <plant name="rose"/>
        </LivingRoom>
        <TerraceGarden>
            <plant name="passion fruit"/>
            <plant name="lily"/>
            <plant name="golden duranta"/>
        </TerraceGarden>
    </House>

XPATH

    count(/House//plant)

OUTPUT

    4.0

