---
title: "Active Model Serializers"
slug: "active-model-serializers"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

ActiveModelSerializers, or AMS for short, bring 'convention over configuration' to your JSON generation.
ActiveModelSerializers work through two components: serializers and adapters.
Serializers describe which attributes and relationships should be serialized.
Adapters describe how attributes and relationships should be serialized.


## Using a serializer
    class SomeSerializer < ActiveModel::Serializer
      attribute :title, key: :name
      attributes :body
    end

