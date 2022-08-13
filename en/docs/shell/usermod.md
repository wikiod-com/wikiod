---
title: "usermod"
slug: "usermod"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## add a user to a group
let's say you have two users with username `chyingp` and `casper`, and want to add them both to group `root`.
 
 ```
 usermod chyingp -a -G root
 usermod casper -a -G root
 ```
 
 here comes the explanation of `-a`, `-G`
 
 ```
 -a, --append
 Add the user to the supplementary group(s). Use only with the -G option.
 -G, --groups GROUP1[,GROUP2,...[,GROUPN]]]
 A list of supplementary groups which the user is also a member of. Each group is separated from the next by
 a comma, with no intervening whitespace. The groups are subject to the same restrictions as the group given
 with the -g option.
 
 If the user is currently a member of a group which is not listed, the user will be removed from the group.
 This behaviour can be changed via the -a option, which appends the user to the current supplementary group
 list.
 ```

