---
title: "Geo"
slug: "geo"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Redis provides the GEO datatype to work with geospatial indexed data.  

## Syntax
- GEOADD key longitude latitude member [longitude latitude member ...]
- GEODIST key member1 member2 [unit]

## GEOADD
The GEOADD command allows a user to add geospatial information (item name, longitude, latitude) to a particular key.

The GEOADD command can be used to add a single item to a key:

```
GEOADD meetup_cities -122.43 37.77 "San Francisco"
```

or multiple items to a key:
```
GEOADD meetup_cities -122.43 37.77 "San Francisco" -104.99 39.74 "Denver"
```

## GEODIST
The GEODIST command allows a user to determine the distance between two members within a geospatial index while specifying the units.

To find the distance between two meetup cities:

    GEODIST meetup_cities "San Francisco" "Denver" mi

