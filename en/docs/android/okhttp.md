---
title: "OkHttp"
slug: "okhttp"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Basic usage example
I like to wrap my `OkHttp` into a class called `HttpClient` for example, and in this class I have methods for each of the major HTTP verbs, `post`, `get`, `put` and `delete`, most commonly. (I usually include an interface, in order to keep for it to implement, in order to be able to easily change to a different implementation, if need be):

    public class HttpClient implements HttpClientInterface{

    private static final String TAG = OkHttpClient.class.getSimpleName();
    public static final MediaType JSON
            = MediaType.parse("application/json; charset=utf-8");

    OkHttpClient httpClient = new OkHttpClient();

    @Override
    public String post(String url, String json) throws IOException {
        Log.i(TAG, "Sending a post request with body:\n" + json + "\n to URL: " + url);

        RequestBody body = RequestBody.create(JSON, json);
        Request request = new Request.Builder()
                .url(url)
                .post(body)
                .build();
        Response response = httpClient.newCall(request).execute();
        return response.body().string();
    }

The syntax is the same for `put`, `get` and `delete` except for 1 word (`.put(body)`) so it might be obnoxious to post that code as well. Usage is pretty simple, just call the appropriate method on some `url` with some `json` payload and the method will return a string as a result that you can later use and parse. Let's assume that the response will be a `json`, we can create a `JSONObject` easily from it:

    String response = httpClient.post(MY_URL, JSON_PAYLOAD);
    JSONObject json = new JSONObject(response);
    // continue to parse the response according to it's structure



## Logging interceptor
`Interceptors` are used to intercept `OkHttp` calls. The reason to intercept could be to monitor, rewrite and retry calls. It can be used for outgoing request or incoming response both.

    class LoggingInterceptor implements Interceptor {
      @Override public Response intercept(Interceptor.Chain chain) throws IOException {
        Request request = chain.request();
    
        long t1 = System.nanoTime();
        logger.info(String.format("Sending request %s on %s%n%s",
            request.url(), chain.connection(), request.headers()));
    
        Response response = chain.proceed(request);
    
        long t2 = System.nanoTime();
        logger.info(String.format("Received response for %s in %.1fms%n%s",
            response.request().url(), (t2 - t1) / 1e6d, response.headers()));
    
        return response;
      }
    }

## Setting up OkHttp
Grab via Maven:

    <dependency>
      <groupId>com.squareup.okhttp3</groupId>
      <artifactId>okhttp</artifactId>
      <version>3.6.0</version>
    </dependency>

or Gradle:

    compile 'com.squareup.okhttp3:okhttp:3.6.0'

## Rewriting Responses


## Synchronous Get Call


## Asynchronous Get Call


## Posting form parameters


## Posting a multipart request


