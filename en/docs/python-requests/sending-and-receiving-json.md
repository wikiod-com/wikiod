---
title: "Sending and receiving JSON"
slug: "sending-and-receiving-json"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

## POSTing JSON
To POST a JSON body, pass in a Python data structure to the `json` argument; here a dictionary is posted but anything that can be encoded to JSON will do:

    import requests

    # Create a dictionary to be sent.
    json_data = {'foo': ['bar', 'baz'], 'spam': True, 'eggs': 5.5}

    # Send the data.
    response = requests.post(url='http://example.com/api/foobar', json=json_data)
    print("Server responded with %s" % response.status_code)

  
`requests` takes care of encoding to JSON for you, and sets the `Content-Type` to `application/json`.



## Receiving JSON in a response
When a response contains valid JSON, just use the `.json()` method on the `Response` object to get the decoded result:

    response = requests.get('http://example.com/')
    decoded_result = response.json()

However, this does not fail gracefully; it will raise a `JSONDecodeError` if the response object is not JSON-parseable.

You may wish to first check the content MIME type, for more graceful error handling:

    if 'application/json' in response.headers['Content-Type']:
        decoded_result = response.json()
    else:
        non_json_result = response.data

## ETL from web API's with modules json and requests
First, import modules and set connection strings. If you need parameters, you can either put them directly in the URL string (an API in this case) or build them as a dict and pass them to the params argument.

    import requests
    import json
    
    params = {'id': 'blahblah', 'output': 'json'} # You could use https://www.somesite.com/api/query?id=blahblah&output=json directly.
    API = 'https://www.somesite.com/api/query'
    APIcred = 'username','password'

Requests handles HTTPBasicAuth and HTTPDigestAuth automatically. This example API will return a JSON string. Make the GET request and capture the output. Raise an error for bad HTTP status.
    
    r = requests.get(API, params = params, auth = APIcred)
    r.raise_for_status()
    #print(r.status) # Optionally print HTTP status code

Convert string of JSON to python object you can work with. JSON looks visually similar to like a python dict, but there are significant differences in nulls, true/false, etc.

    r_dict = json.loads(r.text)
    print(r_dict)

Imagine that the output you just printed comes from a multi-line, multi-column database and is difficult to read:

> {'row': [{'Country': 'United States', 'pid': 'cc12608f-4591-46d7-b8fe-6222e4cde074', 'Status': '', 'FormerLastName': '', 'Degree': 'Business Administration'}, {'Country': 'Britain', 'pid': 'c9f2c6f7-f736-49d3-8adf-fd8d533bbd58', 'Status': '', 'FormerLastName': '', 'Degree': 'General Management'}]}

You can print() a more human-readable version with json.dumps(). The below line encodes the python object to a string of JSON with tabs and prints it.

    print(json.dumps(r_dict, indent = 4))

Output:

    {
        "row": [
            {
                "Country": "United States",
                "pid": "cc12608f-4591-46d7-b8fe-6222e4cde074",
                "Status": "",
                "FormerLastName": "",
                "Degree": "Business Administration"
            },
            {
                "Country": "Britain",
                "pid": "c9f2c6f7-f736-49d3-8adf-fd8d533bbd58",
                "Status": "",
                "FormerLastName": "",
                "Degree": "General Management"
            }
        ]
    }

You can access nested elements in a dict like this:

    print(some_dict['BuildingA']['Room12'])

But our sample has an arbitrary number of objects in an array, which is itself nested as the value of a key! These can be accessed with a row number, starting with 0.

Let's change one of our 'Country' values from 'Britain' to 'Albania':

    r_dict['row'][1]['Country'] = 'Albania'

Now let's send this data to another API. Requests can accept a dict directly to the json argument, as opposed to encoding a string with json.dumps().

    r = requests.post('https://www.somesite.com/" + 'api/carrots', json = r_dict, auth = APIcred)
    r.raise_for_status()

