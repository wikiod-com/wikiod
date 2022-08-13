---
title: "Calling a model method in a view"
slug: "calling-a-model-method-in-a-view"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Sometimes is more usefull make a call to a model's method in our view, so this is a way to make it

## Save a method call in a variable
In Controller:

    $this->load->model('your_model');
    $data['model'] = $this->your_model;

In view:

    $model->your_method;



