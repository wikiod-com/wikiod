---
title: "Middleware"
slug: "middleware"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

In Go Middleware can be used to execute code before and after handler function. It uses the power of Single Function Interfaces.
Can be introduced at any time without affecting the other middleware.
For Ex: Authentication logging can be added in later stages of development without disturbing the existing code.

The **Signature of middleware** should be (http.ResponseWriter, *http.Request) i.e. of
**http.handlerFunc** type.

## Normal Handler Function
    func loginHandler(w http.ResponseWriter, r *http.Request) {
                // Steps to login
    }
    

    func main() {
        http.HandleFunc("/login", loginHandler)
        http.ListenAndServe(":8080", nil)
    }

## Middleware Calculate time required for handlerFunc to execute
    // logger middlerware that logs time taken to process each request
    func Logger(h http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            startTime := time.Now()
            h.ServeHttp(w,r)
            endTime := time.Since(startTime)
            log.Printf("%s %d %v", r.URL, r.Method, endTime)
        })
    }
    
    func loginHandler(w http.ResponseWriter, r *http.Request) {
                // Steps to login
    }
    
    
    func main() {
        http.HandleFunc("/login", Logger(loginHandler))
        http.ListenAndServe(":8080", nil)
    }
        

## CORS Middleware


    func CORS(h http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            origin := r.Header.Get("Origin")
            w.Header().Set("Access-Control-Allow-Origin", origin)
            if r.Method == "OPTIONS" {
                w.Header().Set("Access-Control-Allow-Credentials", "true")
                w.Header().Set("Access-Control-Allow-Methods", "GET,POST")
                
                w.RespWriter.Header().Set("Access-Control-Allow-Headers", "Content-Type, X-CSRF-Token, Authorization")
                return
            } else {
                h.ServeHTTP(w, r)
            }
        })
    }

    func main() {
        http.HandleFunc("/login", Logger(CORS(loginHandler)))
        http.ListenAndServe(":8080", nil)
    }

## Auth Middleware
    func Authenticate(h http.Handler) http.Handler {
        return CustomHandlerFunc(func(w *http.ResponseWriter, r *http.Request) {
            // extract params from req
            // post params | headers etc
            if CheckAuth(params) {
                log.Println("Auth Pass")
                // pass control to next middleware in chain or handler func
                h.ServeHTTP(w, r)
            } else {
                log.Println("Auth Fail")
                // Responsd Auth Fail
            }
        })
    }

## Recovery Handler to prevent server from crashing
    func Recovery(h http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request){
            defer func() {
                if err := recover(); err != nil {
                    // respondInternalServerError
                }
            }()
            h.ServeHTTP(w , r)
        })
    }

