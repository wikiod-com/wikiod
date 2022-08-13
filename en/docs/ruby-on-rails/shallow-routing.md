---
title: "Shallow Routing"
slug: "shallow-routing"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## 1. Use of shallow
One way to avoid deep nesting (as recommended above) is to generate the collection actions scoped under the parent, so as to get a sense of the hierarchy, but to not nest the member actions. In other words, to only build routes with the minimal amount of information to uniquely identify the resource, like this:


    resources :articles, shallow: true do
      resources :comments
      resources :quotes
      resources :drafts
    end

The shallow method of the DSL creates a scope inside of which every nesting is shallow. This generates the same routes as the previous example:

    shallow do
      resources :articles do
        resources :comments
        resources :quotes
        resources :drafts
      end
    end

There exist two options for scope to customize shallow routes. :shallow_path prefixes member paths with the specified parameter:


    scope shallow_path: "sekret" do
      resources :articles do
        resources :comments, shallow: true
      end
    end
Use Rake Command for get generated routes as define below:

    rake routes

