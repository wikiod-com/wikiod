---
title: "Glass Mapper"
slug: "glass-mapper"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Glass.Mapper is the awesome Open Source mapping framework that allows you to focus on solving your business problems. It does the hard work of converting data from your CMS to something your code can work with.

Using Glass.Mapper and your favourite CMS you can map data to strongly typed models in your C# code. Your models do not require any special mark-up to work with Glass.Mapper and almost no configuration to get started. 

Visit this url for tutorials http://glass.lu/Mapper/Sc/Tutorials

## The easiest way to map Sitecore data to code.
Glass.Mapper.Sc allows you to move your data from Sitecore and into your code seamlessly using strongly typed objects.

The framework allows you to map data on to c# classes and interfaces without any additional mark-up. As the data is mapped to your target objects it is converted to the target type. Take a look at this simple example:

    public class Demo
        {
            public virtual Guid Id { get; set; }
    
            public virtual string Title { get; set; }
    
            public virtual DateTime Date { get; set; }
    
            public virtual string Url { get; set; }
        }
    
    
        public void DoWork(
                ISitecoreContext sitecoreContext)
        {
            var model = 
                sitecoreContext.GetCurrentItem<Demo>();
    
            var url = model.Url;
        }



