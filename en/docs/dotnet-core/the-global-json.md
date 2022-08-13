---
title: "The global.json"
slug: "the-globaljson"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

The `global.json` file is [extremely powerful and unique](https://ievangelist.github.io/blog/the-global-json/) to `.NET Core` and `ASP.NET Core` applications.

## Schema
[Schema store:][1]

    {
      "title": "JSON schema for the ASP.NET global configuration files",
      "$schema": "http://json-schema.org/draft-04/schema#",
    
      "type": "object",
      "additionalProperties": true,
      "required": [ "projects" ],
    
      "properties": {
        "projects": {
          "type": "array",
          "description": "A list of project folders relative to this file.",
          "items": {
            "type": "string"
          }
        },
        "packages": {
          "type": "string",
          "description": "The location to store packages"
        },
        "sdk": {
          "type": "object",
          "description": "Specify information about the SDK.",
          "properties": {
            "version": {
              "type": "string",
              "description": "The version of the SDK to use."
            },
            "architecture": {
              "enum": [ "x64", "x86" ],
              "description": "Specify which processor architecture to target."
            },
            "runtime": {
              "enum": [ "clr", "coreclr" ],
              "description": "Chose which runtime to target."
            }
          }
        }
      }
    }


  [1]: http://json.schemastore.org/global

