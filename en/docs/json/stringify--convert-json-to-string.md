---
title: "Stringify - Convert JSON to string"
slug: "stringify---convert-json-to-string"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Parameters
| Param | Details |
| ------ | ------ |
| Object   | (Object) The JSON object   |
| Replacer   | (Function \| Array<string \| number> - optiotinal) filter Function \| Array
| Space   | (Number \| string - optional) Amount of white space in the JSON


## Convert simple JSON object to simple string
    var JSONObject = {
       stringProp: 'stringProp',
       booleanProp: false,
       intProp: 8
    }

    var JSONString = JSON.stringify(JSONObject);
    console.log(JSONString);
    /* output 
     * {"stringProp":"stringProp","booleanProp":false,"intProp":8} 
     */

## Stringify with filter
    var JSONObject = {
       stringProp: 'stringProp',
       booleanProp: false,
       intProp: 8
    }

    var JSONString = JSON.stringify(JSONObject, ['intProp']);
    console.log(JSONString);
    /* output 
     * {"intProp":8} 
     */

## Stringify with white-space
    var JSONObject = {
       stringProp: 'stringProp',
       booleanProp: false,
       intProp: 8
    }

    var JSONString = JSON.stringify(JSONObject, null, 2);
    console.log(JSONString);
    /* output:
     *  {
     *    "stringProp": "stringProp",
     *    "booleanProp": false,
     *    "intProp": 8
     *   }
     */

