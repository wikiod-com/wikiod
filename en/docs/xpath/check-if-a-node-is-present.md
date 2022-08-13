---
title: "Check if a node is present"
slug: "check-if-a-node-is-present"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
 - boolean(path_to_node)

Boolean function has other uses

 1. [Check if a string is empty][1]
 2. Check if the argument is not a number (NaN) or is 0


  [1]: https://www.wikiod.com/xpath/check-if-a-nodes-text-is-empty

## Does the animal have tusks?
XML

    <Animal>
        <legs>4</legs>
        <eyes>2</eyes>
        <horns>2</horns>
        <tail>1</tail>
    </Animal>

XPATH

    boolean(/Animal/tusks)

OUTPUT

    false

## Does the animal have horns?
XPATH

    <Animal>
        <legs>4</legs>
        <eyes>2</eyes>
        <horns>2</horns>
        <tail>1</tail>
    </Animal>

XPATH

    boolean(/Animal/horns)

OUTPUT

    true



## Are there plants in the house?
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

    boolean(/House//plant)

OUTPUT

    true

