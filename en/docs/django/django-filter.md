---
title: "django-filter"
slug: "django-filter"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Use django-filter with CBV
`django-filter` is generic system for filtering Django QuerySets based on user selections. [The documentation][1] uses it in a function-based view as a product model:
```
from django.db import models

class Product(models.Model):
    name = models.CharField(max_length=255)
    price = models.DecimalField()
    description = models.TextField()
    release_date = models.DateField()
    manufacturer = models.ForeignKey(Manufacturer)
```

The filter will be as follows:
```
import django_filters

class ProductFilter(django_filters.FilterSet):
    name = django_filters.CharFilter(lookup_expr='iexact')

    class Meta:
        model = Product
        fields = ['price', 'release_date']
```
To use this in a CBV, override `get_queryset()` of the ListView, then return the filtered `querset`:
```
from django.views.generic import ListView
from .filters import ProductFilter

class ArticleListView(ListView):
    model = Product

    def get_queryset(self):
        qs = self.model.objects.all()
        product_filtered_list = ProductFilter(self.request.GET, queryset=qs)
        return product_filtered_list.qs
```

It is possible to access the filtered objects in your views, such as with pagination, in `f.qs`. This will paginate the filtered objects list.


  [1]: https://django-filter.readthedocs.io/en/latest/

