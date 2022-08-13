---
title: "url and urlRoot"
slug: "url-and-urlroot"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Modifying Model.url()
`Model.url` and `Collection.url` are only used internally by the default `Backbone.sync` method. The default method assumes you are tying into a RESTful API. If you are using a different endpoint design, you will want to override the `sync` method and may want utilize the `url` method.
    
    var Model = Backbone.Model.extend({
      
      urlRoot: '/path-to-model',
      
      url: function (path) {
        var url = this.urlRoot + '/' + path;
        if (this.isNew()) {
          return url;
        }
        return url + '/' + this.get(this.idAttribute);
      }
      
    });
    
    var model = new Model();
    model.url('create'); // /path-to-model/create
    model.set('id', 1);
    model.url('read'); // /path-to-model/read/1
    model.url('update'); // /path-to-model/update/1
    model.url('delete'); // /path-to-model/delete/1

