---
title: "Web Server Gateway Interface (WSGI)"
slug: "web-server-gateway-interface-wsgi"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| start_response | A function used to process the start    |

## Server Object (Method)
Our server object is given an 'application' parameter which can be any callable application object (see other examples).  It writes first the headers, then the body of data returned by our application to the system standard output.
    
    import os, sys    

    def run(application):
        environ['wsgi.input']        = sys.stdin
        environ['wsgi.errors']       = sys.stderr

        headers_set = []
        headers_sent = []

        def write (data):
            """ 
            Writes header data from 'start_response()' as well as body data from 'response' 
            to system standard output. 
            """
            if not headers_set:
                raise AssertionError("write() before start_response()")

            elif not headers_sent:
                status, response_headers = headers_sent[:] = headers_set
                sys.stdout.write('Status: %s\r\n' % status)
                for header in response_headers:
                    sys.stdout.write('%s: %s\r\n' % header)
                sys.stdout.write('\r\n')

            sys.stdout.write(data)
            sys.stdout.flush()

        def start_response(status, response_headers):
            """ Sets headers for the response returned by this server."""
            if headers_set:
                raise AssertionError("Headers already set!")

            headers_set[:] = [status, response_headers]
            return write

        # This is the most important piece of the 'server object'
        # Our result will be generated by the 'application' given to this method as a parameter
        result = application(environ, start_response)
        try:
            for data in result:
                if data:
                    write(data)          # Body isn't empty send its data to 'write()'
            if not headers_sent:
                write('')                # Body is empty, send empty string to 'write()'

