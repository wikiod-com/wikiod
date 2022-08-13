---
title: "Lucene Query Syntax"
slug: "lucene-query-syntax"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Boolean search
`+firstname:john +surname:doe`

Matches documents where firstname is john and surname is doe. **+** predix indicates that the search term *must* occur (AND).

`+firstname:john -surname:doe`

Matches documents where firstname is john and surname is not doe. **-** predix indicates that the search term *must not* occur (NOT).

`+firstname:john surname:(doe bloggs)`

Matches documents where firstname is john and surname is either doe or bloggs. No prefix indicates that the surname *should* occur (OR)

## Basic search
`name:john`

Searches for a single term (joe) in a single field (name)

## Boosting search terms
`name:(john doe^5)`

The **^** indicator can be used to boost a search term to increase it's relevance level meaning that documents containing *doe* are more relevant than ones containing *john*

## Proximity search
`name:"john doe"~1`

Searches for multiple terms within a specific term distance (**~1**), i.e will find text containing **john anonymous doe** but not **john second name doe**

## Phrase search
`name:"john doe"`

Searches for multiple terms in specific order.

## Wildcard search
`name:john*`

The * indicator allows you to do a wildcard search matching 0 or more characters after the search term *john*, will return documents containing john, johnson, john's, johnny and so on.

`name:do?`

The ? indicator allows you to do a wildcard search with a single character in the search term, will return documents containing doe, dog, dot and so on.

## Range search
`age:[50 TO 60]`

Matches documents where age is between 50 and 60 including 50 and 60

`age:{50 TO 60}`

Matches documents where age is between 50 and 60 excluding 50 and 60

`age:[* TO 60]`

Matches documents where age is less than or equal to 60

`age:[50 TO *]`

Matches documents where age is greater than or equal to 50

`age:{50 to 60]`

You can mix curly and square brackets. Matches documents where age is greather than 50 but less than or equal to 60



## Join across cores
`{!join from=personid to=id fromIndex=AddressCore}address:Address1`

So if you have two cores that look like this:

**PersonCore** - id, name

**AddressCore** - id, address, personid

This will find all PersonCore documents at a specific address



