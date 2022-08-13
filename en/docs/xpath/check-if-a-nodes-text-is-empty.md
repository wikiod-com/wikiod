---
title: "Check if a node's text is empty"
slug: "check-if-a-nodes-text-is-empty"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntax
 - boolean(path_to_node/text())
 - string(path_to_node) != ''

Boolean function has other uses

 1. [Check if a node is present][1]
 2. Check if the argument is not a number (NaN) or is 0

String function is used to return the string value of a node.


  [1]: https://www.wikiod.com/xpath/check-if-a-node-is-present

## Check if Deborah has a master and its text value is not empty
XML

    <Deborah>
        <address>Dark world</address>
        <master>Babadi</master>
        <ID>#0</ID>
        <colour>red</colour>
        <side>evil</side>
    </Deborah>


XPATH

    boolean(/Deborah/master/text())

   OR

    string(/Deborah/master) != ''

OUTPUT

    true

## Check if Dobby has a master and its text value is not empty
XML

    <Dobby>
        <address>Hogwartz</address>
        <master></master>
        <colour>wheatish</colour>
        <side>all good</side>
    </Dobby>

XPATH

    boolean(/Dobby/master/text())

OR

    string(/Dobby/master) != ''

OUTPUT

    false

