---
title: "Templates in python"
slug: "templates-in-python"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Simple data output program using template
    from string import Template
    
    data = dict(item = "candy", price = 8, qty = 2)
    
    # define the template
    t = Template("Simon bought $qty $item for $price dollar")   
    print(t.substitute(data))

Output:

    Simon bought 2 candy for 8 dollar

Templates support $-based substitutions instead of %-based substitution. **Substitute** (mapping, keywords) performs template substitution, returning a new string.

Mapping is any dictionary-like object with keys that match with the template placeholders. In this example, price and qty are placeholders. Keyword arguments can also be used as placeholders. Placeholders from keywords take precedence if both are present.

## Changing delimiter
You can change the "$" delimiter to any other. The following example:

    from string import Template
    
    class MyOtherTemplate(Template):
        delimiter = "#"
    
    
    data = dict(id = 1, name = "Ricardo")
    t = MyOtherTemplate("My name is #name and I have the id: #id")
    print(t.substitute(data))

You can read de docs [here](https://docs.python.org/3/library/string.html?highlight=template#string.Template.template)

