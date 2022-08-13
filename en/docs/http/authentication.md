---
title: "Authentication"
slug: "authentication"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| ------- | ------- |
| Response status | [`401`](https://tools.ietf.org/html/rfc7235#section-3.1) if the origin server requires authentication, [`407`](https://tools.ietf.org/html/rfc7235#section-3.2) if an intermediate proxy requires authentication |
| Response headers | [`WWW-Authenticate`](https://tools.ietf.org/html/rfc7235#section-4.1) by the origin server, [`Proxy-Authenticate`](https://tools.ietf.org/html/rfc7235#section-4.3) by an intermediate proxy |
| Request headers | [`Authorization`](https://tools.ietf.org/html/rfc7235#section-4.2)  for authorization against an origin server, [`Proxy-Authorization`](https://tools.ietf.org/html/rfc7235#section-4.4) against an intermediate proxy |
| Authentication scheme | `Basic` for Basic Authentication, but others such as `Digest` and `SPNEGO` can be used. See the [HTTP Authentication Schemes Registry](https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml).
| Realm | A name of the protected space on the server; a server can have multiple such spaces, each with a distinct name and authentication mechanisms. |
| Credentials | For `Basic`: username and password separated by a colon, base64-encoded; for example, `username:password` base64-encoded is `dXNlcm5hbWU6cGFzc3dvcmQ=`

Basic Authentication is defined in [RFC2617](https://www.ietf.org/rfc/rfc2617.txt). It can be used to authenticate against the origin server after receiving a ``401 Unauthorized`` as well as against a proxy server after a ``407 (Proxy Authentication Required)``. In the (decoded) credentials, the password starts after the first colon. Therefore the username cannot contain a colon, but the password can.

## HTTP Basic Authentication
HTTP Basic Authentication provides a straightforward mechanism for authentication. Credentials are sent in plain text, and so is insecure by default. Successful authentication proceeds as follows.

The client requests a page for which access is restricted:
```
GET /secret
```

The server responds with status code ``401 Unauthorized`` and requests the client to authenticate:
```
401 Unauthorized
WWW-Authenticate: Basic realm="Secret Page"
```

The client sends the ``Authorization`` header. The credentials are ``username:password`` base64 encoded:
```
GET /secret
Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=
```

The server accepts the credentials and responds with the page content:
```
HTTP/1.1 200 OK
```

