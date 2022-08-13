---
title: "Callbacks"
slug: "callbacks"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Interpreting errors with "error" callback
Errors, when managed properly by the server, will be returned to your client with a specific HTTP status code different from 2xx (see [RFC 2616 section 10][1]).

It's advised to catch globally your errors from your `$.ajaxSetup()` as demonstrated in the example below. Therefore all errors coming from your ajax calls will be automatically interpreted from the ajax setup.

    $.ajaxSetup({
        error: function (jqXHR, exception, errorThrown) {
            var message;
            var statusErrorMap = {
                '400': "Server understood the request, but request content was invalid.",
                '401': "Unauthorized access.",
                '403': "Forbidden resource can't be accessed.",
                '500': "Internal server error.",
                '503': "Service unavailable."
            };
            if (jqXHR.status) {
                message = statusErrorMap[jqXHR.status];
                if (!message) {
                    message = "Unknown Error.";
                }
            } else if (exception == 'parsererror') {
                message = "Error.\nParsing JSON Request failed.";
            } else if (exception == 'timeout') {
                message = "Request Time out.";
            } else if (exception == 'abort') {
                message = "Request was aborted by the server";
            } else {
                message = "Unknown Error.";
            }
    
            // How you will display your error message...
            console.log(message);
            console.log(errorThrown);
        }
    });

You may also want to "overload" the `error` callback in a specific `$.ajax()` when you are waiting for a specific error message.

    $.ajax({
        url: './api',
        data: { parametersObject },
        type:'post',
        dataType: 'json',
        success:function(output){
            // Interpret success
        },
        error: function(xhr,textStatus,ErrorThrown){
            // Specific error will not be interpreted by $.ajaxSetup
        }
    });

  [1]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

