---
title: "Serializers"
slug: "serializers"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

According to DRF official documentation, serializers helps to convert complex data like querysets and model instance to native python data types so that it could be rendered as JSON, XML and other content types.

The serializers in DRF are more like django **Form** and **ModelForm** class. **Serializer** class provide us a custom way to handle the data like django Form. And **ModelSerializer** class provide us a easy way to handle model based data.

## Basic Introduction
Let's say we have a model called product.

    class Product(models.Model):
        name = models.CharField(max_length=100)
        price = models.IntegerField()

Now we are going to declare a model serializers for this model.

    from rest_framework.serializers import ModelSerializer
    
    class ProductSerializers(ModelSerializer):
        class Meta:
            model = Product
            fields = '__all__'
            read_only_fields = ('id',)
By this ProductSerializers class we have declared a model serializers. In Meta class, by model variable we told the ModelSerializer that our model will be the Product model. By fields variable we told that this serializer should include all the field of the model. Lastly by read_only_fields variable we told that id will be a 'read only' field, it can't be edited.

Let's see what's in our serializer. At first, import the serializer in command line and create a instance and then print it.

    >>> serializer = ProductSerializers()
    >>> print(serializer)
    ProductSerializers():
            id = IntegerField(label='ID', read_only=True)
            name = CharField(max_length=100)
            price = IntegerField(max_value=2147483647, min_value=-2147483648)
So, our serializer grab all the field form our model and create all the field it's own way.

We can use ProductSerializers to serialize a product, or a list of product.

    >>> p1 = Product.objects.create(name='alu', price=10)
    >>> p2 = Product.objects.create(name='mula', price=5)
    >>> serializer = ProductSerializers(p1)
    >>> print(serializer.data)
    {'id': 1,'name': 'alu', 'price': 10}
At this point we've translated the model instance into Python native datatypes. To finalize the serialization process we render the data into json.

    >>> from rest_framework.renderers import JSONRenderer
    >>> serializer = ProductSerializers(p1)
    >>> json = JSONRenderer().render(serializer.data)
    >>> print(json)
    '{"id": 1,"name": "alu", "price": 10}'

 

