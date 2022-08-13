---
title: "Getting started with python-requests"
slug: "getting-started-with-python-requests"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
`python-requests` is available on PyPI, the Python Package Index, which means it can be installed through pip:

    pip install requests

Up-to-date source code can be found on the [requests GitHub repository][1]

If you wish to install it from source, you can do this by either cloning the GitHub repository:

    git clone git://github.com/kennethreitz/requests.git

Or by getting the tarball (`-O` writes the output to file; `-L` follows redirects): 

    curl -OL https://github.com/kennethreitz/requests/tarball/master
    
Then you can install it by executing the `setup.py`

    python setup.py install

However you installed it, you can start using it by importing the usual way

    >>> import requests
    >>> requests.get('http://stackoverflow.com')

  [1]: https://github.com/kennethreitz/requests

## GET requests
`requests.get()` creates a GET request:

    response = requests.get('https://example.com/')

Pass in query parameters as a dictionary to the `params` argument:

    response = requests.get('https://example.com/', params={"a": 1, "b": 2})

For GET requests that might require basic authentication, you can include the `auth` paramter as follows:

    response = requests.get('https://api.github.com/user', auth=('user', 'pass'))


## POST requests
POST requests are made with the `request.post()` method.

If you need to send a web form request as a POST body, pass in a dictionary with key-value  pairs as the `data` argument; `requests` will encode these to a `application/x-www-form-urlencoded` mimetype body:

    r = requests.post('https://github.com/', data={"a": 1, "b": 2})

If you need to POST a json payload, you can use `json=`.  This will automatically set the Content-Type header to `application/json`

    r = requests.post('https://github.com/', data={"a": 1, "b": 2})

    



## Other request methods
The `requests` module has top-level functions for most HTTP methods:

    r = requests.put('https://example.com/', data=put_body)
    r = requests.delete('https://example.com/')
    r = requests.head('https://example.com/')
    r = requests.options('https://example.com/')
    r = requests.patch('https://example.com/', data=patch_update)

## Reading the response
    response = requests.get("https://api.github.com/events")
    text_resp = response.text

**JSON response**: for json-formatted responses the package provides a built-in decoder

    response = requests.get('https://api.github.com/events')
    json_resp = response.json()
This method will raise a `ValueError` in case of empty response or unparseable content.

## Reading status codes
The attribute `status_code` contains the status code of the response

    good_req = requests.get('https://api.github.com/events')
    code_200 = good_req.status_code
    
    notfound_req = requests.get('https://api.github.com/not_found')
    code_404 = notfound_req.status_code

`requests.codes.__dict__` will provide a list of available http status codes.

It is possible to user `raise_for_status` to check if the status_code was 4xx or 5xx and raise a corresponding exception in that case.

    good_req = requests.get('https://api.github.com/events')
    good_req.raise_for_status()
    # is a 200 status code so nothing happens
    
    notfound_req = requests.get('https://api.github.com/not_found')
    notfound_req.raise_for_status()
    # raises requests.exceptions.HTTPError: 404 Client Error


