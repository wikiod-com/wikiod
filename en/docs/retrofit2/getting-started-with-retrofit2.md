---
title: "Getting started with retrofit2"
slug: "getting-started-with-retrofit2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
**What is retrofit?** 

The [official Retrofit page][1] describes itself as:

> A type-safe REST client for Android and Java.

This library makes downloading **JSON** or **XML** data from a web API fairly straightforward. Once the data is downloaded then it is parsed into a Plain Old Java Object (POJO) defined for each request using anyone of the adapter/parser listed [here][2].

For Demo purpose we would be using [GSON][3] parser

**Setup**:

1. Add internet permission in manifest.xml:

<!-- language: xml -->

     <uses-permission android:name="android.permission.INTERNET" />

2. Add the following to your `build.gradle` file:

<!-- language: java -->

    dependencies {
        compile 'com.squareup.retrofit2:retrofit:2.1.0'
        compile 'com.squareup.retrofit2:converter-gson:2.1.0'  
    }

3. Create proper POJO(Model) based on your **Json** response:

    If your json response is:

<!-- language: js -->

    {
        "CategoryModel": {
            "debug": "on",
            "window": {
                "title": "Sample Konfabulator Widget",
                "name": "main_window",
                "width": 500,
                "height": 500
            },
            "image": {
                "src": "Images/Sun.png",
                "name": "sun1",
                "hOffset": 250,
                "vOffset": 250,
                "alignment": "center"
            },
            "text": {
                "data": "Click Here",
                "size": 36,
                "style": "bold",
                "name": "text1",
                "hOffset": 250,
                "vOffset": 100,
                "alignment": "center",
                "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
            }
        }
    }

Then you can use website like [JsonOnlineEditor][4] or [JsonView][5] to format your json which will help to create your model Or use [jsonschema2pojo][6] to convert your Json to POJO using [GSON][3] annotations : 

<!-- language: java -->

    public class CategoryModel {
       
        @SerializedName("debug")
        private String debug;

        @SerializedName("window")
        private Window window;

        @SerializedName("image")
        private Image image;

        @SerializedName("text")
        private Text text;
    }


4. Then we need an instance of Retrofit which acts as controller for all the request and response. 

    Note : We prefer to create this controller as singleton which is very helpful if we want to set some additional property of the client . 

<!-- language: java -->

    public static final String BASE_URL = "http://test.com"
    
    Retrofit retrofit = new Retrofit.Builder()
                .baseUrl(BASE_URL)
                .addConverterFactory(GsonConverterFactory.create())
                .build();
        

5. Next create **Interface** class where define all api calls with request,response type and request params for each call .(We need to create an interface for managing url calls like `GET`,`POST`..etc.)

<!-- language: java -->

    public interface IPlusService {
        @GET("/api/category")
        Call<CategoryModel> getAllCategory();
    }

6. Create network/request client with retrofit instance :

<!-- language: java -->

    IPlusService requestClient = retrofit.create(IPlusService.class);
 
7. Call your web-service in your `Fragment/Activity`: 

<!-- language: java -->

    requestClient.getAllCategory().enqueue(new Callback<ResponseBody>() {
        @Override
        public void onResponse(Call<CategoryModel> call, Response<CategoryModel> response) {
            // DO success handling 
        }

        @Override
        public void onFailure(Call<CategoryModel> call, Throwable t) {
            // DO failure handling 
        }
    });



**Good resources:**

1. https://github.com/codepath/android_guides/wiki/Consuming-APIs-with-Retrofit

2. http://www.vogella.com/tutorials/Retrofit/article.html

3. http://www.androidhive.info/2016/05/android-working-with-retrofit-http-library/

4. https://futurestud.io/tutorials/retrofit-getting-started-and-android-client


  [1]: https://square.github.io/retrofit/
  [2]: https://github.com/square/retrofit/tree/master/retrofit-converters
  [3]: https://github.com/google/gson
  [4]: http://www.jsoneditoronline.org
  [5]: http://jsonviewer.stack.hu/
  [6]: http://www.jsonschema2pojo.org/

