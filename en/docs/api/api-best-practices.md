---
title: "Api Best Practices"
slug: "api-best-practices"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

These are some API Best Practices I had been used.

## Parameters
| Parameter     | Details                                               |
| ------------- | ----------------------------------------------------- |
| **sort**      | Sort by Single or Multiple columns. `sort=email,name` |
| **dir**       | Sort Ascending/Descending. `dir=asc` or `dir=desc`    |
| **column**    | Filter by. `phone=5555555555`                         |
| **limit**     | Items per Page. `limit=25`                            |
| **page**      | Page Number. `page=2`                                 |
| **api_key**   | API Key. `api_key=ABCDEF1234567890`                   |

# Status Codes

## 1xx: Information
| Message | Description | Long Description |
| ------ | ------ | ------|
|100| Continue|The server has received the request headers, and the client should proceed to send the request body
|101| Switching Protocols|The requester has asked the server to switch protocols
|103| Checkpoint|Used in the resumable requests proposal to resume aborted PUT or POST requests

## 2xx: Successful
| Message | Description | Long Description |
| ------ | ------ | ------|
|200| OK|The request is OK (this is the standard response for successful HTTP requests)
|201| Created|The request has been fulfilled, and a new resource is created 
|202| Accepted|The request has been accepted for processing, but the processing has not been completed
|203| Non-Authoritative Information|The request has been successfully processed, but is returning information that may be from another source
|204| No Content|The request has been successfully processed, but is not returning any content
|205| Reset Content|The request has been successfully processed, but is not returning any content, and requires that the requester reset the document view
|206| Partial Content|The server is delivering only part of the resource due to a range header sent by the client

# 3xx: Redirection
| Message | Description | Long Description |
| ------ | ------ | ------|
|300| Multiple Choices|A link list. The user can select a link and go to that location. Maximum five addresses  |
|301| Moved Permanently|The requested page has moved to a new URL |
|302| Found|The requested page has moved temporarily to a new URL |
|303| See Other|The requested page can be found under a different URL|
|304| Not Modified|Indicates the requested page has not been modified since last requested|
|306| Switch Proxy|No longer used|
|307| Temporary Redirect|The requested page has moved temporarily to a new URL|
|308| Resume Incomplete|Used in the resumable requests proposal to resume aborted PUT or POST requests|

## 4xx: Client Error
| Message | Description | Long Description |
| ------ | ------ | ------|
|400| Bad Request|The request cannot be fulfilled due to bad syntax
|401| Unauthorized|The request was a legal request, but the server is refusing to respond to it. For use when authentication is possible but has failed or not yet been provided
|402| Payment Required|Reserved for future use
|403| Forbidden|The request was a legal request, but the server is refusing to respond to it
|404| Not Found|The requested page could not be found but may be available again in the future
|405| Method Not Allowed|A request was made of a page using a request method not supported by that page
|406| Not Acceptable|The server can only generate a response that is not accepted by the client
|407| Proxy Authentication Required|The client must first authenticate itself with the proxy
|408| Request Timeout|The server timed out waiting for the request
|409| Conflict|The request could not be completed because of a conflict in the request
|410| Gone|The requested page is no longer available
|411| Length Required|The "Content-Length" is not defined. The server will not accept the request without it 
|412| Precondition Failed|The precondition given in the request evaluated to false by the server
|413| Request Entity Too Large|The server will not accept the request, because the request entity is too large
|414| Request-URI Too Long|The server will not accept the request, because the URL is too long. Occurs when you convert a POST request to a GET request with a long query information 
|415| Unsupported Media Type|The server will not accept the request, because the media type is not supported 
|416| Requested Range Not Satisfiable|The client has asked for a portion of the file, but the server cannot supply that portion
|417| Expectation Failed|The server cannot meet the requirements of the Expect request-header field

## 5xx: Server Error
| Message | Description | Long Description |
| ------ | ------ | ------|
|500|Internal Server Error|A generic error message, given when no more specific message is suitable
|501|Not Implemented|The server either does not recognize the request method, or it lacks the ability to fulfill the request
|502|Bad Gateway|The server was acting as a gateway or proxy and received an invalid response from the upstream server
|503|Service Unavailable|The server is currently unavailable (overloaded or down)
|504|Gateway Timeout|The server was acting as a gateway or proxy and did not receive a timely response from the upstream server
|505|HTTP Version Not Supported|The server does not support the HTTP protocol version used in the request
|511|Network Authentication Required|The client needs to authenticate to gain network access

Reference: [Http Codes][1]

[1]: https://www.w3schools.com/tags/ref_httpmessages.asp

## 200 - Success Response
    {
        "status": 200,
        "message": "OK",
        "data": [
            {
                "name": "dignissimos",
                "description": "Maxime rerum molestias error a consequatur adipisci inventore corrupti.",
                "category": 8,
                "created_at": "2017-04-29 07:27:19"
            },
            {
                "name": "placeat",
                "description": "Ipsum soluta numquam qui et vitae perferendis eligendi nostrum.",
                "category": 7,
                "created_at": "2017-04-29 07:28:36"
            },
            {
                "name": "inventore",
                "description": "Dolorem repudiandae consectetur porro aspernatur ut ad autem.",
                "category": 6,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "eaque",
                "description": "Repudiandae molestias et aut.",
                "category": 9,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "omnis",
                "description": "Voluptas iusto in ut omnis.",
                "category": 1,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "qui",
                "description": "Quia consequatur dolorem quasi maiores accusantium et officia.",
                "category": 3,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "voluptatem",
                "description": "Ipsa molestiae quo tempore ea excepturi.",
                "category": 1,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "optio",
                "description": "Corporis non ipsam velit fugiat necessitatibus modi minima.",
                "category": 8,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "illo",
                "description": "Ut ratione quisquam aut sed enim.",
                "category": 1,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "natus",
                "description": "Id deserunt et numquam esse voluptatem quam sit.",
                "category": 9,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "voluptatem",
                "description": "Debitis amet eos quia eaque neque molestiae veniam.",
                "category": 2,
                "created_at": "2017-04-29 07:28:39"
            },
            {
                "name": "sapiente",
                "description": "Harum tempore quod eum.",
                "category": 3,
                "created_at": "2017-04-29 07:28:40"
            },
            {
                "name": "qui",
                "description": "Mollitia voluptas consequatur atque nobis quisquam animi est sint.",
                "category": 5,
                "created_at": "2017-04-29 07:28:40"
            },
            {
                "name": "impedit",
                "description": "Veritatis ut suscipit tenetur tempora qui eum tempora.",
                "category": 10,
                "created_at": "2017-04-29 07:28:40"
            },
            {
                "name": "ut",
                "description": "Libero quaerat quod laboriosam est rerum.",
                "category": 10,
                "created_at": "2017-04-29 07:28:40"
            }
        ],
        "meta": {
            "category": "products",
            "pagination": {
                "total": 102,
                "count": 15,
                "per_page": 15,
                "current_page": 1,
                "total_pages": 7,
                "links": {
                    "next": "http://api.mydomian.com/api/v1/products?page=2"
                }
            }
        }
    
    }

## 400 - Bad Request Error Response
    {
        "status": 400,
        "message": "Bad Request",
        "errors": [
            {
                "code": "E100",
                "message": "Missing First Name",
                "field": "first_name",
            },
            {
                "code": "E101",
                "message": "Invalid Email Address",
                "field": "email",
            }
        ]
    }

## 401 - Unauthorized Error Response
    {
        "status": 401,
        "message": "Unauthorized",
        "errors": []
    }

