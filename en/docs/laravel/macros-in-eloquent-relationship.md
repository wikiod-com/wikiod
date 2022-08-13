---
title: "Macros In Eloquent Relationship"
slug: "macros-in-eloquent-relationship"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

We have new features for Eloquent Relationship in Laravel version 5.4.8. We can fetch a single instance of a hasMany (it is just one example) relationship by define it at on place and it will works for all relationship 

## We can fetch one instance of hasMany relationship
In our AppServiceProvider.php

    public function boot()
    {
      HasMany::macro('toHasOne', function() {
          return new HasOne(
              $this->query,
              $this->parent,
              $this->foreignKey,
              $this->localKey
          );
      });
    }

Suppose we have shop modal and we are getting the list of products which has purchased. Suppose we have allPurchased relationship for Shop modal

    public function allPurchased()
    {
        return $this->hasMany(Purchased::class);
    }

    public function lastPurchased()
    {
        return $this->allPurchased()->latest()->toHasOne();
    }

