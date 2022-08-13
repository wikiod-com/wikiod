---
title: "ijson"
slug: "ijson"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

ijson is a great library for working with JSON files in Python. Unfortunately, by default it uses a pure Python JSON parser as its backend. Much higher performance can be achieved by using a C backend.

## Simple Example
Sample Example Taken from one [benchmarking][1] 


    import ijson
    
    def load_json(filename):
        with open(filename, 'r') as fd:
            parser = ijson.parse(fd)
            ret = {'builders': {}}
            for prefix, event, value in parser:
                if (prefix, event) == ('builders', 'map_key'):
                    buildername = value
                    ret['builders'][buildername] = {}
                elif prefix.endswith('.shortname'):
                    ret['builders'][buildername]['shortname'] = value
    
            return ret
    
    if __name__ == "__main__":
        load_json('allthethings.json')
 
JSON FILE [LINK][2]


  [1]: http://explique.me/Ijson/
  [2]: https://secure.pub.build.mozilla.org/builddata/reports/allthethings.json

