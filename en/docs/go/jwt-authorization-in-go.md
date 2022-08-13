---
title: "JWT Authorization in Go"
slug: "jwt-authorization-in-go"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

JSON Web Tokens (JWTs) are a popular method for representing claims securely between two parties. Understanding how to work with them is important when developing web applications or application programming interfaces.

context.Context and HTTP middleware are outside the scope of this topic, but nonetheless those curious, wandering souls should check out https://github.com/goware/jwtauth, https://github.com/auth0/go-jwt-middleware, and https://github.com/dgrijalva/jwt-go.

Huge kudos to Dave Grijalva for his amazing work on go-jwt.

## Parsing and validating a token using the HMAC signing method
    // sample token string taken from the New example
    tokenString := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYXIiLCJuYmYiOjE0NDQ0Nzg0MDB9.u1riaD1rW97opCoAuRCTy4w58Br-Zk-bh7vLiRIsrpU"
    
    // Parse takes the token string and a function for looking up the key. The latter is especially
    // useful if you use multiple keys for your application.  The standard is to use 'kid' in the
    // head of the token to identify which key to use, but the parsed token (head and claims) is provided
    // to the callback, providing flexibility.
    token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
        // Don't forget to validate the alg is what you expect:
        if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
            return nil, fmt.Errorf("Unexpected signing method: %v", token.Header["alg"])
        }
    
        // hmacSampleSecret is a []byte containing your secret, e.g. []byte("my_secret_key")
        return hmacSampleSecret, nil
    })
    
    if claims, ok := token.Claims.(jwt.MapClaims); ok && token.Valid {
        fmt.Println(claims["foo"], claims["nbf"])
    } else {
        fmt.Println(err)
    }

Output:

    bar 1.4444784e+09

(From the [documentation](https://godoc.org/github.com/dgrijalva/jwt-go#ex-Parse--Hmac), courtesy of Dave Grijalva.)

## Creating a token using a custom claims type
The `StandardClaim` is embedded in the custom type to allow for easy encoding, parsing and validation of standard claims.

    tokenString := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYXIiLCJleHAiOjE1MDAwLCJpc3MiOiJ0ZXN0In0.HE7fK0xOQwFEr4WDgRWj4teRPZ6i3GLwD5YCm6Pwu_c"
    
    type MyCustomClaims struct {
        Foo string `json:"foo"`
        jwt.StandardClaims
    }
    
    // sample token is expired.  override time so it parses as valid
    at(time.Unix(0, 0), func() {
        token, err := jwt.ParseWithClaims(tokenString, &MyCustomClaims{}, func(token *jwt.Token) (interface{}, error) {
            return []byte("AllYourBase"), nil
        })
    
        if claims, ok := token.Claims.(*MyCustomClaims); ok && token.Valid {
            fmt.Printf("%v %v", claims.Foo, claims.StandardClaims.ExpiresAt)
        } else {
            fmt.Println(err)
        }
    })

Output:

    bar 15000

(From the [documentation](https://godoc.org/github.com/dgrijalva/jwt-go#ex-ParseWithClaims--CustomClaimsType), courtesy of Dave Grijalva.)

## Creating, signing, and encoding a JWT token using the HMAC signing method
    // Create a new token object, specifying signing method and the claims
    // you would like it to contain.
    token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
        "foo": "bar",
        "nbf": time.Date(2015, 10, 10, 12, 0, 0, 0, time.UTC).Unix(),
    })
    
    // Sign and get the complete encoded token as a string using the secret
    tokenString, err := token.SignedString(hmacSampleSecret)
    
    fmt.Println(tokenString, err)

Output:

    eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYXIiLCJuYmYiOjE0NDQ0Nzg0MDB9.u1riaD1rW97opCoAuRCTy4w58Br-Zk-bh7vLiRIsrpU <nil>

(From the [documentation](https://godoc.org/github.com/dgrijalva/jwt-go#ex-New--Hmac), courtesy of Dave Grijalva.)

## Using the StandardClaims type by itself to parse a token
The `StandardClaims` type is designed to be embedded into your custom types to provide standard validation features. You can use it alone, but there's no way to retrieve other fields after parsing. See the custom claims example for intended usage.

    mySigningKey := []byte("AllYourBase")
    
    // Create the Claims
    claims := &jwt.StandardClaims{
        ExpiresAt: 15000,
        Issuer:    "test",
    }
    
    token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
    ss, err := token.SignedString(mySigningKey)
    fmt.Printf("%v %v", ss, err)

Output:

    eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE1MDAwLCJpc3MiOiJ0ZXN0In0.QsODzZu3lUZMVdhbO76u3Jv02iYCvEHcYVUI1kOWEU0 <nil>

(From the [documentation](https://godoc.org/github.com/dgrijalva/jwt-go#ex-NewWithClaims--StandardClaims), courtesy of Dave Grijalva.)

## Parsing the error types using bitfield checks
    // Token from another example.  This token is expired
    var tokenString = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYXIiLCJleHAiOjE1MDAwLCJpc3MiOiJ0ZXN0In0.HE7fK0xOQwFEr4WDgRWj4teRPZ6i3GLwD5YCm6Pwu_c"
    
    token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
        return []byte("AllYourBase"), nil
    })
    
    if token.Valid {
        fmt.Println("You look nice today")
    } else if ve, ok := err.(*jwt.ValidationError); ok {
        if ve.Errors&jwt.ValidationErrorMalformed != 0 {
            fmt.Println("That's not even a token")
        } else if ve.Errors&(jwt.ValidationErrorExpired|jwt.ValidationErrorNotValidYet) != 0 {
            // Token is either expired or not active yet
            fmt.Println("Timing is everything")
        } else {
            fmt.Println("Couldn't handle this token:", err)
        }
    } else {
        fmt.Println("Couldn't handle this token:", err)
    }

Output:

    Timing is everything

(From the [documentation](https://godoc.org/github.com/dgrijalva/jwt-go#ex-Parse--ErrorChecking), courtesy of Dave Grijalva.)

## Getting token from HTTP Authorization header
    type contextKey string
    
    const (
        // JWTTokenContextKey holds the key used to store a JWT Token in the
        // context.
        JWTTokenContextKey contextKey = "JWTToken"
    
        // JWTClaimsContextKey holds the key used to store the JWT Claims in the
        // context.
        JWTClaimsContextKey contextKey = "JWTClaims"
    )

    // ToHTTPContext moves JWT token from request header to context.
    func ToHTTPContext() http.RequestFunc {
        return func(ctx context.Context, r *stdhttp.Request) context.Context {
            token, ok := extractTokenFromAuthHeader(r.Header.Get("Authorization"))
            if !ok {
                return ctx
            }
    
            return context.WithValue(ctx, JWTTokenContextKey, token)
        }
    }

(From [go-kit/kit](https://github.com/go-kit/kit/blob/master/auth/jwt/transport.go), courtesy of Peter Bourgon)

