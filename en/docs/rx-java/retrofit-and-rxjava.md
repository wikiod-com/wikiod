---
title: "Retrofit and RxJava"
slug: "retrofit-and-rxjava"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Set up Retrofit and RxJava
Retrofit2 comes with support for multiple pluggable execution mechanisms, one of them is RxJava.

To use retrofit with RxJava you first need to add the Retrofit RxJava adapter to your project:

    compile 'com.squareup.retrofit2:adapter-rxjava:2.1.0'
then you need to add the adapter when building your retrofit instance:

    Retrofit retrofit = new Retrofit.Builder()
        .baseUrl("https://api.example.com")
        .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
        .build();
In your interface when you define the API the return type should be `Observable` eg:

    public interface GitHubService {
      @GET("users/{user}/repos")
      Observable<List<Repo>> listRepos(@Path("user") String user);
    }
You can also use `Single` instead of `Observable`.

 

## Making serial requests
RxJava is handy when making serial request. If you want to use the result from one request to make another you can use the `flatMap` operator:

    api.getRepo(repoId).flatMap(repo -> api.getUser(repo.getOwnerId())
        .subscribe(/*do something with the result*/);

## Making parallel requests 
You can use the `zip` operator to make request in parallel and combine the results eg:

    Observable.zip(api.getRepo(repoId1), api.getRepo(repoId2), (repo1, repo2) ->
        {
            //here you can combine the results
        }).subscribe(/*do something with the result*/);

