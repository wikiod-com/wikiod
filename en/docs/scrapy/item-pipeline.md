---
title: "Item Pipeline"
slug: "item-pipeline"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Way to process every item that Scrapy outputs.

An Item Pipeline is a python class that overrides some specific methods and needs to be activated on the `settings` of the scrapy project.

## Creating a dynamic pipeline in Python Scrapy
Enable pipelines in your **`settings.py`**
    
    ITEM_PIPELINES = {
        'project_folder.pipelines.MyPipeline': 100 
    }


Then write this code in **`items.py`**

    # -*- coding: utf-8 -*-
    from scrapy import Item, Field
    from collections import OrderedDict

    class DynamicItem(Item):
        def __setitem__(self, key, value):
            self._values[key] = value
            self.fields[key] = {}

Then in your `project_folder/spiders/spider_file.py

    from project_folder.items import DynamicItem
           def parse(self, response):
                   # create an ordered dictionary
                   data = OrderedDict()
                   data['first'] = ...
                   data['second'] = ...
                   data['third'] = ...
                   .
                   .
                   .
                   # create dictionary as long as you need
                   
                   # now unpack dictionary
                   yield DynamicItem( **data )

                   # above line is same as this line
                   yield DynamicItem( first = data['first'], second = data['second'], third = data['third'])


<h3>**What are benefits of this code?**</h3>

No need to create define each item in `items.py` one by one.



## Creating your own Pipeline
When creating a scrapy project with `scrapy startproject myproject`, you'll find a `pipelines.py` file already available for creating your own pipelines. It isn't mandatory to create your pipelines in this file, but it would be good practice. We'll be explaining how to create a pipeline using the `pipelines.py` file:

**pipelines.py**

    class MyPipeline(object):
        def process_item(self, item, spider):
            # process your `item` here
            return item

Now to enable it you need to specify it is going to be used in your settings. Go to your `settings.py` file and search (or add) the `ITEM_PIPELINES` variable. Update it with the path to your pipeline class and its priority over other pipelines:

**settings.py**


    ITEM_PIPELINES = {
        'myproject.pipelines.MyPipeline': 300,
    }

Now every item that your spider returns, will go through this pipeline.

