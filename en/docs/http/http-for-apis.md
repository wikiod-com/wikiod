---
title: "HTTP for APIs"
slug: "http-for-apis"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

HTTP APIs use a wide spectrum of HTTP verbs and typically return JSON or XML responses.

## Create a resource
Not everyone agrees on what the most semantically correct method for resource creation is. Thus, your API could accept `POST` or `PUT` requests, or either.

The server should respond with `201 Created` if the resource was successfully created. Pick the most appropriate error code if it was not.

For example, if you provide an API to create employee records, the request/response might look like this:

    POST /employees HTTP/1.1
    Host: example.com
    Content-Type: application/json
    
    {
        "name": "Charlie Smith",
        "age": 38,
        "job_title": "Software Developer",
        "salary": 54895.00
    }

<!-- -->

    HTTP/1.1 201 Created
    Location: /employees/1/charlie-smith
    Content-Type: application/json
    
    {
        "employee": {
            "name": "Charlie Smith",
            "age": 38,
            "job_title": "Software Developer",
            "salary": 54895.00
            "links": [
                {
                    "uri": "/employees/1/charlie-smith",
                    "rel": "self",
                    "method": "GET"
                },
                {
                    "uri": "/employees/1/charlie-smith",
                    "rel": "delete",
                    "method": "DELETE"
                },
                {
                    "uri": "/employees/1/charlie-smith",
                    "rel": "edit",
                    "method": "PATCH"
                }
            ]
        },
        "links": [
            {
                "uri": "/employees",
                "rel": "create",
                "method": "POST"
            }
        ]
    }

Including the `links` JSON fields in the response enables the client to access resource related to the new resource and to the application as a whole, without having to know their URIs or methods beforehand.

## Edit a resource
Editing or updating a resource is a common purpose for APIs. Edits can be achieved by sending either `POST`, `PUT` or `PATCH` requests to the respective resource. Although `POST` is [allowed to append data to a resource's existing representation][7] it is recommended to use either `PUT` or `PATCH` as they convey a more explicit semantic.

Your server should respond with `200 OK` if the update was performed, or `202 Accepted` if it has yet to be applied. Pick the most appropriate error code if it cannot be completed.

# Full updates

`PUT` has the semantics of replacing the current representation with the payload included in the request.  If the payload is not of the same representation type as the current representation of the resource to update, the server can decide which approach to take. [RFC7231][1] defines that the server can either 

* Reconfigure the target resource to reflect the new media type
* Transform the PUT representation to a format consistent with that of the resouce before saving it as the new resource state
* Reject the request with a `415 Unsupported Media Type` response indicating that the target resource is limited to a specific (set) of media types.

A base resource containing a JSON [HAL][2] representation like ...

    {
        "name": "Charlie Smith",
        "age": 39,
        "job_title": "Software Developer",
        "_links": {
            "self": { "href": "/users/1234" },
            "employee": { "href": "http://www.acmee.com" },
            "curies": [{ "name": "ea", "href": "http://www.acmee.com/docs/rels/{rel}", templated": true}],
            "ea:admin": [
                "href": "/admin/2",
                "title": "Admin"
            ]
        }
    }

... may receive an update request like this

    PUT /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Content-Type: "application/json; charset=utf-8"
    Content-Length: 85
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)

    {
        "name": "Charlie Gold-Smith",
        "age": 40,
        "job_title": "Senior Software Developer"
    }

The server may now replace the state of the resource with the given request body and also change the content-type from `application/hal+json` to `application/json` or convert the JSON payload to a JSON HAL representation and then replace the content of the resource with the transformed one or reject the update request due to an inaplicable media type with a `415 Unsupported Media Type` response.

There is a difference between replacing the content directly or first transforming the representation to the defined representation model and then replacing the existing content with the transformed one. A subsequent `GET` request will return the following response on a direct replacement:

    GET /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Accept-Encoding: gzip, deflate
    Accept-Language: en-us
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
    ETag: "e0023aa4e"

    {
        "name": "Charlie Gold-Smith",
        "age": 40,
        "job_title": "Senior Software Developer"
    }

while the transformation and then replace approach will return the following representation:


    GET /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Accept-Encoding: gzip, deflate
    Accept-Language: en-us
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
    ETag: e0023aa4e

    {
        "name": "Charlie Gold-Smith",
        "age": 40,
        "job_title": "Senior Software Developer",
        "_links": {
            "self": { "href": "/users/1234" },
            "employee": { "href": "http://www.acmee.com" },
            "curies": [{ "name": "ea", "href": "http://www.acmee.com/docs/rels/{rel}", templated": true}],
            "ea:admin": [
                "href": "/admin/2",
                "title": "Admin"
            ]
        }
    }

### Side-Effects

Note that `PUT` is allowed to have side-effects although it is defined as idempotent operation! This is documented in [RFC7231][1] as

> A PUT request applied to the target resource **can have side effects on other resources**.  For example, an article might have a URI for identifying "the current version" (a resource) that is separate from the URIs identifying each particular version (different resources  that at one point shared the same state as the current version resource).  A successful PUT request on "the current version" URI might therefore create a new version resource in addition to changing the state of the target resource, and might also cause links to be added between the related resources.

Producing additional log entries is not considered as side effect usually as this is certainly no state of a resource in general.

# Partial updates

[RFC7231][1] mentions this regarding partial updates:

> Partial content updates are possible by targeting a separately identified resource with state that overlaps a portion of the larger resource, or by using a different method that has been specifically defined for partial updates (for example, the PATCH method defined in [RFC5789][3]).

Partial updates can therefore be performed in two flavors:

* Have a resource embed multiple smaller sub-resources and update only a respective sub-resource instead of the full resource via `PUT`
* Using `PATCH` and [instruct the server what to update][4]

## Partial update with overlapping state

If a user representation needs to be partially updated due to a move of a user to an other location, instead of updating the user directly, the related resource should be updated directly which reflects to a partial update of the user representation.

Before the move a user had the following representation

    GET /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Accept: application/hal+json; charset=utf-8
    Accept-Encoding: gzip, deflate
    Accept-Language: en-us
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
    ETag: "e0023aa4e"

    {
        "name": "Charlie Gold-Smith",
        "age": 40,
        "job_title": "Senior Software Developer",
        "_links": {
            "self": { "href": "/users/1234" },
            "employee": { "href": "http://www.acmee.com" },
            "curies": [{ "name": "ea", "href": "http://www.acmee.com/docs/rels/{rel}", templated": true}],
            "ea:admin": [
                "href": "/admin/2",
                "title": "Admin"
            ]
        },
        "_embedded": {
            "ea:address": {
                "street": "Terrace Drive, Central Park",
                "zip": "NY 10024"
                "city": "New York",
                "country": "United States of America",
                "_links": {
                    "self": { "href": "/address/abc" },
                    "google_maps": { "href": "http://maps.google.com/?ll=40.7739166,-73.970176" }
                }
            }
        }
    }

As the user is moving to a new location she updates her location information like this:

    PUT /address/abc HTTP/1.1
    Host: http://www.acmee.com
    Content-Type: "application/json; charset=utf-8"
    Content-Length: 109
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)

    {
        "street": "Standford Ave",
        "zip": "CA 94306",
        "city": "Pablo Alto",
        "country": "United States of America"
    }

With the transformation-before-replace semantic for the mismatched media-type between the existing address resource and the one in the request, as described above, the address resource is now updated which has the effect that on a subsequent `GET` request on the user resource the new address for the user is returned.

    GET /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Accept: application/hal+json; charset=utf-8
    Accept-Encoding: gzip, deflate
    Accept-Language: en-us
    User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)
    ETag: "e0023aa4e"

    {
        "name": "Charlie Gold-Smith",
        "age": 40,
        "job_title": "Senior Software Developer",
        "_links": {
            "self": { "href": "/users/1234" },
            "employee": { "href": "http://www.acmee.com" },
            "curies": [{ "name": "ea", "href": "http://www.acmee.com/docs/rels/{rel}", templated": true}],
            "ea:admin": [
                "href": "/admin/2",
                "title": "Admin"
            ]
        },
        "_embedded": {
            "ea:address": {
                "street": "Standford Ave",
                "zip": "CA 94306",
                "city": "Pablo Alto",
                "country": "United States of America"
                "_links": {
                    "self": { "href": "/address/abc" },
                    "google_maps": { "href": "http://maps.google.com/?ll=37.4241311,-122.1524475" }
                }
            }
        }
    }

## Patching partial data

`PATCH` is defined in [RFC5789][3] and is not directly part of the HTTP spec per se. A common misbelief is, that [sending only the fields that should be partially updated is enough][4] within a `PATCH` request. The specification therefore states

> The PATCH method requests that a set of changes described in the request entity be applied to the resource identified by the Request-URI.  The set of changes is represented in a format called a "patch document" identified by a media type.

This means that a client should calculate the necessary steps needed to transform the resource from state A to state B and send these instructions to the server. 

A popular JSON based media-type for patching is [JSON Patch][5].

If the age and the job-title of our sample user changes and an additional field representing the income of the user should be added a partial update using `PATCH` using JSON Patch may look like this:

    PATCH /users/1234 HTTP/1.1
    Host: http://www.acmee.com
    Content-Type: application/json-patch+json; charset=utf-8
    Content-Length: 188
    Accept: application/json
    If-Match: "e0023aa4e"

    [
        { "op": "replace", "path": "/age", "value": 40 },
        { "op": "replace", "path": "/job_title", "value": "Senior Software Developer" },
        { "op": "add", "path": "/salery", "value": 63985.00 }
    ]

`PATCH` may update multiple resources at once and requires to apply the changes atomically which means either all changes have to be applied or none at all which puts transactional burden on the API implementor.

A successful update may return something like this

    HTTP/1.1 200 OK
    Location: /users/1234
    Content-Type: application/json
    ETag: "df00eb258"

    {
        "name": "Charlie Smith",
        "age": 40,
        "job_title": "Senior Software Developer",
        "salary": 63985.00
    }

though is not restricted to `200 OK` response codes only.

To prevent in-between updates (changes done in-between the previous fetch of the representation state and the update) `ETag`, `If-Match` or `If-Unmodified-Since` header should be used.

### Error Handling

The spec on `PATCH` recommends the following error handling:

| Type | Error Code |
| ------ | ------ |
| Malformed patch document   | `400 Bad Request`   |
| Unsupported patch document | `415 Unsupported Media Type` |
| Unprocessable request, i.e. if the resoure would become invalid by applying the patch | `422 Unprocessable Entity` |
| Resource not found | `404 Not Found` |
| Conflicting state, i.e. a rename (move) of a field which does not exist | `409 Conflict` |
| Conflicting modification, i.e. if the client uses a `If-Match` or `If-Unmodified-Since` header which validation failed. If no precondition was available, the latter error code should be returned | `412 Precondition Failed` or `409 Conflict` |
| Concurrent modification, i.e. if the request needs to be applied before acception further `PATCH` requests | `409 Conflict` |


  [1]: https://tools.ietf.org/html/rfc7231#section-4.3.4
  [2]: http://stateless.co/hal_specification.html
  [3]: https://tools.ietf.org/html/rfc5789
  [4]: http://williamdurand.fr/2014/02/14/please-do-not-patch-like-an-idiot/
  [5]: https://tools.ietf.org/html/rfc6902
  [6]: http://jsonpatch.com/
  [7]: https://tools.ietf.org/html/rfc7231#section-4.3.3

## Delete a resource
Another common use of HTTP APIs is to delete an existing resource. This should usually be done using `DELETE` requests.

If the deletion was successful, the server should return `200 OK`; an appropriate error code if it was not.

If our employee Charlie Smith has left the company and we now want to delete his records, that might look like this:

    DELETE /employees/1/charlie-smith HTTP/1.1
    Host: example.com

<!-- -->

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
        'links': [
            {
                'uri': '/employees',
                'rel': 'create',
                'method': 'POST'
            }
        ]
    }

## List resources
The last common use of HTTP APIs is to obtain a list of existing resources on the server. Lists like this should be obtained using `GET` requests, since they only *retrieve* data.

The server should return `200 OK` if it can supply the list, or an appropriate error code if not.

Listing our employees, then, might look like this:

    GET /employees HTTP/1.1
    Host: example.com

<!-- -->

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
        'employees': [
            {
                'name': 'Charlie Smith',
                'age': 39,
                'job_title': 'Software Developer',
                'salary': 63985.00
                'links': [
                    {
                        'uri': '/employees/1/charlie-smith',
                        'rel': 'self',
                        'method': 'GET'
                    },
                    {
                        'uri': '/employees/1/charlie-smith',
                        'rel': 'delete',
                        'method': 'DELETE'
                    },
                    {
                        'uri': '/employees/1/charlie-smith',
                        'rel': 'edit',
                        'method': 'PATCH'
                    }
                ]
            },
            {
                'name': 'Donna Prima',
                'age': 30,
                'job_title': 'QA Tester',
                'salary': 77095.00
                'links': [
                    {
                        'uri': '/employees/2/donna-prima',
                        'rel': 'self',
                        'method': 'GET'
                    },
                    {
                        'uri': '/employees/2/donna-prima',
                        'rel': 'delete',
                        'method': 'DELETE'
                    },
                    {
                        'uri': '/employees/2/donna-prima',
                        'rel': 'edit',
                        'method': 'PATCH'
                    }
                ]
            }
        ],
        'links': [
            {
                'uri': '/employees/new',
                'rel': 'create',
                'method': 'PUT'
            }
        ]
    }

