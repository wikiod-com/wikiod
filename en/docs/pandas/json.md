---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Read JSON 
# can either pass string of the json, or a filepath to a file with valid json
    In [99]: pd.read_json('[{"A": 1, "B": 2}, {"A": 3, "B": 4}]')
    Out[99]:
       A  B
    0  1  2
    1  3  4

Alternatively to conserve memory:

    with open('test.json') as f:
        data = pd.DataFrame(json.loads(line) for line in f)

## Dataframe into nested JSON as in flare.js  files used in D3.js
    def to_flare_json(df, filename):
        """Convert dataframe into nested JSON as in flare files used for D3.js"""
        flare = dict()
        d = {"name":"flare", "children": []}
        
        for index, row in df.iterrows():
            parent = row[0]
            child = row[1]
            child_size = row[2]
    
            # Make a list of keys
            key_list = []
            for item in d['children']:
                key_list.append(item['name'])
    
            #if 'parent' is NOT a key in flare.JSON, append it
            if not parent in key_list:
                d['children'].append({"name": parent, "children":[{"value": child_size, "name": child}]})
            # if parent IS a key in flare.json, add a new child to it
            else:
                d['children'][key_list.index(parent)]['children'].append({"value": child_size, "name": child})
        flare = d
        # export the final result to a json file
        with open(filename +'.json', 'w') as outfile:
            json.dump(flare, outfile, indent=4)
        return ("Done")

## Read JSON from file
Content of file.json (one JSON object per line):

    {"A": 1, "B": 2}
    {"A": 3, "B": 4}

How to read directly from a local file:

    pd.read_json('file.json', lines=True)
    # Output:
    #    A  B
    # 0  1  2
    # 1  3  4

