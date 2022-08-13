---
title: "Getting started with jwt"
slug: "getting-started-with-jwt"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Unsigned JWT
An unsigned JWT has the header value `alg: none` and an empty JWS (signature) component:

    eyJhbGciOiJub25lIn0
    .eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ
    .

The trailing dot indicates that the signature is empty.

## Header
<!-- language: lang-js -->
    {
      "alg": "none"
    }

## Payload
<!-- language: lang-js -->
    {
      "iss": "joe",
      "exp": 1300819380,
      "http://example.com/is_root": true
    }

## Signed JWT (JWS)
A signed JWT includes a Base64 Url Safe encoded signature as the third component. The algorithm used to generate the signature is indicated in the header.

    eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9
    .eyJzdWIiOiJKb2huIERvZSIsImFkbWluIjp0cnVlLCJpYXQiOjE0NzAzNTM5OTQsImV4cCI6MTQ3MDM1NzYyNywianRpIjoiNmU0MDRiYTgtZjg4NS00ZDVmLWJmYTItZTNmNWEwODM4MGE0In0
    .7CfBdVP4uKsb0cogYepCvMLm8rcpjBYW1XZzA-a5e44

## Header
<!-- language: lang-js -->
    {
      "typ": "JWT",
      "alg": "HS256"
    }

This JWT was signed with the HMAC-SHA256 algorithm, hence `alg: HS256`.

## Payload
<!-- language: lang-js -->
    {
      "sub": "John Doe",
      "admin": true,
      "iat": 1470353994,
      "exp": 1470357627,
      "jti": "6e404ba8-f885-4d5f-bfa2-e3f5a08380a4"
    }

This JWT can be verified with the UTF-8 secret `notsosecret`.

## How to tell if you have a JWS or JWE?
From Section 9 of JSON Web Encryption specification (RFC 7516):
> The JOSE Header for a JWS can be distinguished from the JOSE Header for a JWE by examining the "alg" (algorithm) Header Parameter value. If the value represents a digital signature or MAC algorithm, or is the value "none", it is for a JWS; if it represents a Key Encryption, Key Wrapping, Direct Key Agreement, Key Agreement with Key Wrapping, or Direct Encryption algorithm, it is for a JWE. (Extracting the "alg" value to examine is straightforward when using the JWS Compact Serialization or the JWE Compact Serialization and may be more difficult when using the JWS JSON Serialization or the JWE JSON Serialization.)

And 

> The JOSE Header for a JWS can also be distinguished from the JOSE Header for a JWE by determining whether an "enc" (encryption algorithm) member exists. If the "enc" member exists, it is a JWE; otherwise, it is a JWS.

## JWS  (signed)##

    {
      "alg": "HS256"
    }

## JWE (encrypted) ##

    {
      "alg":"RSA1_5",
      "enc":"A256GCM",
      "iv":"__79_Pv6-fg",
      "x5t":"7noOPq-hJ1_hCnvWh6IeYI2w9Q0"
    }



## JSON Web Encryption (JWE)
JSON Web Encryption (JWE) represents encrypted content using JavaScript Object Notation (JSON) based data structures. It defines a way to encrypt your claims data  so that only intended receiver can read the information present in a token.

In the JWE JSON Serialization, a JWE is represented as a JSON object containing some or all of these eight members:

      "protected", with the value BASE64URL(UTF8(JWE Protected Header))
      "unprotected", with the value JWE Shared Unprotected Header
      "header", with the value JWE Per-Recipient Unprotected Header
      "encrypted_key", with the value BASE64URL(JWE Encrypted Key)
      "iv", with the value BASE64URL(JWE Initialization Vector)
      "ciphertext", with the value BASE64URL(JWE Ciphertext)
      "tag", with the value BASE64URL(JWE Authentication Tag)
      "aad", with the value BASE64URL(JWE AAD)

The six base64url-encoded result strings and the two unprotected JSON
   object values are represented as members within a JSON object.

**Example JWE**

The following example JWE Header declares that:
- the Content Encryption Key is encrypted to the recipient using the RSA-PKCS1_1.5 algorithm to produce the JWE Encrypted Key
- the Plaintext is encrypted using the AES-256-GCM algorithm to produce the JWE Ciphertext
- the specified 64-bit Initialization Vector with the base64url encoding __79_Pv6-fg was used
- the thumbprint of the X.509 certificate that corresponds to the key used to encrypt the JWE has the base64url encoding 7noOPq-hJ1_hCnvWh6IeYI2w9Q0.


    {
     "alg":"RSA1_5",
     "enc":"A256GCM",
     "iv":"__79_Pv6-fg",
     "x5t":"7noOPq-hJ1_hCnvWh6IeYI2w9Q0"
    }

Base64url encoding the bytes of the UTF-8 representation of the JWE Header yields this Encoded JWE Header value (with line breaks for display purposes only):

    eyJhbGciOiJSU0ExXzUiLA0KICJlbmMiOiJBMjU2R0NNIiwNCiAiaXYiOiJfXzc5
    X1B2Ni1mZyIsDQogIng1dCI6Ijdub09QcS1oSjFfaENudldoNkllWUkydzlRMCJ

 Read [JSON Web Encryption specification (RFC 7516)][1] for more information


  [1]: https://tools.ietf.org/html/rfc7516

## What to store in a JWT
The JWT [RFC][1] stablish three classes of claims:

- **Registered claims** like `sub`, `iss`, `exp` or `nbf`

- **Public claims** with public names or names [registered by IANA][2] which contain values that should be unique like `email`, `address` or `phone_number`. See [full list][3]

- **Private claims** to use in your own context and values can collision

None of these claims are mandatory

A JWT is self-contained and should avoid use the server session providing the necessary data to perform the authentication (no need of server storage and database access). Therefore, `role` or `permissions` info can be included in private claims of JWT. 

Registered Claims
-----------------
The following Claim Names are registered in the IANA "JSON Web Token Claims" registry established by [Section 10.1.][2]

 - `iss` (issuer): identifies the principal that issued the JWT.
 - `sub` (subject): identifies the principal that is the subject of the JWT. Must be unique
 - `aud` (audience): identifies the recipients that the JWT is intended for (array of strings/uri)
 - `exp` (expiration time): identifies the expiration time (UTC Unix) after which you must no longer accept this token. It should be after the issued-at time.
 - `nbf`(not before): identifies the UTC Unix time before which the JWT must not be accepted   
 - `iat` (issued at): identifies the UTC Unix time at which the JWT was issued
 - `jti` (JWT ID): provides a unique identifier for the JWT.  

Example
-----------------

    {
        "iss": "stackoverflow",
        "sub": "joe",
        "aud": ["all"],
        "iat": 1300819370,
        "exp": 1300819380,
        "jti": "3F2504E0-4F89-11D3-9A0C-0305E82C3301"
        "context": {
            "user": {
                "key": "joe",
                "displayName": "Joe Smith"
            },
            "roles":["admin","finaluser"]
        }
    }


[1]: https://tools.ietf.org/html/rfc7519#page-8
  [2]: https://tools.ietf.org/html/rfc7519#section-10.1
  [3]: http://www.iana.org/assignments/jwt/jwt.xhtml

