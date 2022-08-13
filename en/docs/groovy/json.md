---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Parse a json string
    import groovy.json.JsonSlurper;
    
    def jsonSlurper = new JsonSlurper()
    def obj = jsonSlurper.parseText('{ "foo": "bar", "baz": [1] }')
    
    assert obj.foo == 'bar'
    assert obj.baz == [1]


## Pretty-print a json string
    import groovy.json.JsonOutput;
    
    def json = JsonOutput.toJson([foo: 'bar', baz: [1]])
    
    assert json == '{"foo":"bar","baz":[1]}'

    def pretty = JsonOutput.prettyPrint(json)

    assert pretty == '''{
        "foo": "bar",
        "baz": [
            1
        ]
    }'''

## Parse a json file
    import groovy.json.JsonSlurper;

    def jsonSlurper = new JsonSlurper()

    File fl = new File('/path/to/fils.json')
    
    // parse(File file) method is available since 2.2.0
    def obj = jsonSlurper.parse(fl)

    // for versions < 2.2.0 it's possible to use
    def old = jsonSlurper.parse(fl.text)

## Write a json to string
    

    import groovy.json.JsonOutput;
    
    def json = JsonOutput.toJson([foo: 'bar', baz: [1]])
    
    assert json == '{"foo":"bar","baz":[1]}'

In addition to maps, lists and primitives `groovy.json.JsonOutput` also supports a *POJOs* serialitzation:

```
import groovy.json.JsonOutput; 
 
class Tree { 
    def name
    def type
}

Tree willow = new Tree(name:'Willow',type:'Deciduous')
Tree olive = new Tree(name:'Olive',type:'Evergreen')

assert JsonOutput.toJson(willow) == '{"type":"Deciduous","name":"Willow"}'
assert JsonOutput.toJson([willow,olive]) == '[{"type":"Deciduous","name":"Willow"},{"type":"Evergreen","name":"Olive"}]'
```




## Write a json to a file

    
    import groovy.json.JsonOutput;

    def json = JsonOutput.toJson([foo: 'bar', baz: [1]])
     
    new File("/tmp/output.json").write(json)

