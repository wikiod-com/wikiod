---
title: "Getting started with cypher"
slug: "getting-started-with-cypher"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Keyword Guide
**Read Keywords**

| KeyWord | Function| Example |
| ------ | ------ | ------ |
| MATCH | Find following expression in graph | MATCH (n) |

**Write Keywords**

| KeyWord | Function| Example |
| ------ | ------ | ------ |
| CREATE | Create the following pattern | CREATE (n:Person{name:"Bob"}) |
| DELETE | Delete the following nodes/relationships | DELETE n |
| DETACH DELETE | Delete the following nodes, and any attached relationships | DETACH DELETE n |

**Read-Write Keywords**

| KeyWord | Function| Example |
| ------ | ------ | ------ |
| Merge | Match following pattern, or create it | Merge (n:Person{id:1337}) |

**Filter Keywords**

| KeyWord | Function| Example |
| ------ | ------ | ------ |
| Limit | Limit result rows to the following number. Combine with Skip to page results | Limit 25 |
| Skip | Skip first n result rows. Combine with Limit to page results | Skip 25 |
| WHERE | Filter results by following expression | WHERE n.age > 21 |
| AND/OR | And/Or multiple expressions | WHERE n.age > 21 AND n.age < 30 |
| NOT | Negate following expression | WHERE NOT n.age > 21 |
| ANY/ALL/NONE/SINGLE | Filter based on collection | WHERE ALL (p in people | WHERE p.age > 21) |


Remember to check the Refcard for your version of Cypher, as these may have changed.

 - [current][1]
 - [3.2][2]
 - [3.1][3]
 - [3.0][4]


  [1]: http://neo4j.com/docs/cypher-refcard/current/
  [2]: http://neo4j.com/docs/cypher-refcard/3.2/
  [3]: http://neo4j.com/docs/cypher-refcard/3.1/
  [4]: http://neo4j.com/docs/cypher-refcard/3.0/

