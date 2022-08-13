---
title: "Getting started with jsonschema"
slug: "getting-started-with-jsonschema"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic example validation schema
```json
{
    "title": "Person",
    "type": "object",
    "properties": {
        "firstName": {
            "type": "string"
        },
        "lastName": {
            "type": "string"
        },
        "age": {
            "description": "Age in years",
            "type": "integer",
            "minimum": 0
        }
    },
    "required": ["firstName", "lastName"]
}
```
Results
```json
// Valid
{
    "firstName": "Jason",
    "lastName": "Voorhees"
}
// Valid
{
    "firstName": "Jason",
    "lastName": "Voorhees",
    "age": 47
}
// Invalid - no lastName
{
    "firstName": "Jason",
    "age": 47
}
// Invalid - age is not integer
{
    "firstName": "Jason",
    "lastName": "Voorhees",
    "age": "47"
}
```

